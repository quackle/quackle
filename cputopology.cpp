/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2019 Jason Katz-Brown, John O'Laughlin, and John Fultz.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include <thread>

#include "cputopology.h"

// Define QUACKLE_CPUTOPOLOGY_FORCE_GENERIC to compile out every platform probe
// and exercise the generic fallback path.
#ifndef QUACKLE_CPUTOPOLOGY_FORCE_GENERIC
#	if defined(__APPLE__)
#		define QUACKLE_CPUTOPOLOGY_MACOS 1
#	elif defined(_WIN32)
#		define QUACKLE_CPUTOPOLOGY_WINDOWS 1
#	elif defined(__linux__)
#		define QUACKLE_CPUTOPOLOGY_LINUX 1
#	endif
#endif

#if defined(QUACKLE_CPUTOPOLOGY_MACOS)
#	include <sys/sysctl.h>
#	include <sys/types.h>
#endif

#if defined(QUACKLE_CPUTOPOLOGY_WINDOWS)
#	include <windows.h>
#	include <vector>
#endif

#if defined(QUACKLE_CPUTOPOLOGY_LINUX)
#	include <dirent.h>
#	include <sched.h>
#	include <cstdio>
#	include <cstdlib>
#	include <fstream>
#	include <map>
#	include <set>
#	include <string>
#	include <vector>
#endif

using namespace Quackle;

CPUTopology::CPUTopology()
	: m_logicalCores(0)
	, m_physicalCores(0)
	, m_performanceLogicalCores(0)
	, m_performancePhysicalCores(0)
	, m_efficiencyLogicalCores(0)
	, m_efficiencyPhysicalCores(0)
	, m_hybrid(false)
	, m_genericFallback(false)
{
	if (!probePlatform())
		applyGenericFallback();

	deriveCounts();
}

#if defined(QUACKLE_CPUTOPOLOGY_MACOS)

static bool sysctlInt(const char *name, int *out)
{
	int value = 0;
	size_t size = sizeof(value);
	if (sysctlbyname(name, &value, &size, nullptr, 0) != 0)
		return false;

	*out = value;
	return true;
}

bool CPUTopology::probePlatform()
{
	if (!sysctlInt("hw.logicalcpu", &m_logicalCores) || m_logicalCores <= 0)
		return false;

	if (!sysctlInt("hw.physicalcpu", &m_physicalCores))
		m_physicalCores = 0;

	// Apple silicon reports one "perflevel" per class of core, ordered fastest
	// first, so perflevel0 is the performance cores and everything after it is
	// progressively slower. Intel Macs report a single level (or none at all).
	int perfLevels = 1;
	if (sysctlInt("hw.nperflevels", &perfLevels) && perfLevels > 1)
	{
		int logical = 0;
		int physical = 0;
		if (sysctlInt("hw.perflevel0.logicalcpu", &logical) && sysctlInt("hw.perflevel0.physicalcpu", &physical))
		{
			m_performanceLogicalCores = logical;
			m_performancePhysicalCores = physical;

			for (int level = 1; level < perfLevels; ++level)
			{
				char name[64];
				snprintf(name, sizeof(name), "hw.perflevel%d.logicalcpu", level);
				if (sysctlInt(name, &logical))
					m_efficiencyLogicalCores += logical;
				snprintf(name, sizeof(name), "hw.perflevel%d.physicalcpu", level);
				if (sysctlInt(name, &physical))
					m_efficiencyPhysicalCores += physical;
			}

			m_hybrid = m_performanceLogicalCores > 0 && m_efficiencyLogicalCores > 0;
		}
	}

	m_probeName = MARK_UV("macOS sysctl");
	return true;
}

#elif defined(QUACKLE_CPUTOPOLOGY_WINDOWS)

static int popcountAffinity(KAFFINITY mask)
{
	int count = 0;
	while (mask != 0)
	{
		count += (int)(mask & 1);
		mask >>= 1;
	}

	return count;
}

bool CPUTopology::probePlatform()
{
	DWORD length = 0;
	if (GetLogicalProcessorInformationEx(RelationProcessorCore, nullptr, &length) || GetLastError() != ERROR_INSUFFICIENT_BUFFER)
		return false;

	std::vector<unsigned char> buffer(length);
	auto *info = reinterpret_cast<SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX *>(buffer.data());
	if (!GetLogicalProcessorInformationEx(RelationProcessorCore, info, &length))
		return false;

	// One record per physical core. EfficiencyClass ranks cores by performance,
	// with higher meaning faster; a machine using more than one class is hybrid,
	// and the fastest class is the performance cores.
	struct Core
	{
		int logical;
		BYTE efficiencyClass;
	};

	std::vector<Core> cores;
	BYTE maxEfficiencyClass = 0;
	BYTE minEfficiencyClass = 0xff;

	DWORD offset = 0;
	while (offset < length)
	{
		auto *record = reinterpret_cast<SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX *>(buffer.data() + offset);
		if (record->Size == 0)
			break;

		if (record->Relationship == RelationProcessorCore)
		{
			Core core = { 0, record->Processor.EfficiencyClass };
			for (WORD i = 0; i < record->Processor.GroupCount; ++i)
				core.logical += popcountAffinity(record->Processor.GroupMask[i].Mask);

			if (core.logical > 0)
			{
				if (core.efficiencyClass > maxEfficiencyClass)
					maxEfficiencyClass = core.efficiencyClass;
				if (core.efficiencyClass < minEfficiencyClass)
					minEfficiencyClass = core.efficiencyClass;

				cores.push_back(core);
			}
		}

		offset += record->Size;
	}

	if (cores.empty())
		return false;

	m_physicalCores = (int)cores.size();
	for (const Core &core : cores)
		m_logicalCores += core.logical;

	if (maxEfficiencyClass != minEfficiencyClass)
	{
		for (const Core &core : cores)
		{
			if (core.efficiencyClass == maxEfficiencyClass)
			{
				m_performanceLogicalCores += core.logical;
				++m_performancePhysicalCores;
			}
			else
			{
				m_efficiencyLogicalCores += core.logical;
				++m_efficiencyPhysicalCores;
			}
		}

		m_hybrid = true;
	}

	m_probeName = MARK_UV("Windows GetLogicalProcessorInformationEx");
	return true;
}

#elif defined(QUACKLE_CPUTOPOLOGY_LINUX)

namespace
{

// Identifies a physical core; SMT siblings share one of these.
struct PhysicalCoreId
{
	int package;
	int core;

	bool operator<(const PhysicalCoreId &other) const { return package != other.package ? package < other.package : core < other.core; }
};

bool readSysfsString(const std::string &path, std::string *out)
{
	std::ifstream file(path.c_str());
	if (!file)
		return false;

	std::string line;
	if (!std::getline(file, line))
		return false;

	*out = line;
	return true;
}

bool readSysfsLong(const std::string &path, long *out)
{
	std::string contents;
	if (!readSysfsString(path, &contents))
		return false;

	char *end = nullptr;
	const long value = strtol(contents.c_str(), &end, 10);
	if (end == contents.c_str())
		return false;

	*out = value;
	return true;
}

// Parses a sysfs cpu list, e.g. "0-7,16,20-23".
std::set<int> parseCpuList(const std::string &list)
{
	std::set<int> cpus;
	size_t position = 0;
	while (position < list.size())
	{
		size_t comma = list.find(',', position);
		if (comma == std::string::npos)
			comma = list.size();

		const std::string range = list.substr(position, comma - position);
		if (!range.empty())
		{
			const size_t dash = range.find('-');
			const int first = atoi(range.c_str());
			const int last = dash == std::string::npos ? first : atoi(range.c_str() + dash + 1);
			for (int cpu = first; cpu <= last; ++cpu)
				cpus.insert(cpu);
		}

		position = comma + 1;
	}

	return cpus;
}

std::string cpuPath(int cpu, const char *suffix)
{
	return "/sys/devices/system/cpu/cpu" + std::to_string(cpu) + suffix;
}

}

bool CPUTopology::probePlatform()
{
	std::string onlineList;
	if (!readSysfsString("/sys/devices/system/cpu/online", &onlineList))
		return false;

	std::set<int> cpus = parseCpuList(onlineList);
	if (cpus.empty())
		return false;

	// Respect the affinity mask we were actually given -- a process pinned with
	// taskset or a cpuset shouldn't size its pools for the whole machine. (A
	// cgroup CPU *quota*, as opposed to a cpuset, is not visible here and is not
	// accounted for.)
	cpu_set_t affinity;
	CPU_ZERO(&affinity);
	if (sched_getaffinity(0, sizeof(affinity), &affinity) == 0)
	{
		std::set<int> allowed;
		for (std::set<int>::const_iterator it = cpus.begin(); it != cpus.end(); ++it)
		{
			if (*it < CPU_SETSIZE && CPU_ISSET(*it, &affinity))
				allowed.insert(*it);
		}

		if (!allowed.empty())
			cpus.swap(allowed);
	}

	// Performance class of each cpu: 1 for performance, 0 for efficiency, -1 if
	// we couldn't tell. Intel's hybrid PMUs name their cpus outright; ARM
	// big.LITTLE exposes a capacity per cpu; failing both, maximum clock is the
	// only hint available.
	std::map<int, int> cpuClass;
	std::string performanceList;
	std::string efficiencyList;
	if (readSysfsString("/sys/devices/cpu_core/cpus", &performanceList) && readSysfsString("/sys/devices/cpu_atom/cpus", &efficiencyList))
	{
		const std::set<int> performanceCpus = parseCpuList(performanceList);
		const std::set<int> efficiencyCpus = parseCpuList(efficiencyList);
		for (std::set<int>::const_iterator it = cpus.begin(); it != cpus.end(); ++it)
		{
			if (performanceCpus.count(*it) > 0)
				cpuClass[*it] = 1;
			else if (efficiencyCpus.count(*it) > 0)
				cpuClass[*it] = 0;
		}
	}
	else
	{
		static const char *const rankSources[] = { "/cpu_capacity", "/cpufreq/cpuinfo_max_freq" };
		for (const char *source : rankSources)
		{
			std::map<int, long> ranks;
			long best = 0;
			bool differ = false;
			for (std::set<int>::const_iterator it = cpus.begin(); it != cpus.end(); ++it)
			{
				long rank = 0;
				if (!readSysfsLong(cpuPath(*it, source), &rank))
				{
					ranks.clear();
					break;
				}

				if (!ranks.empty() && rank != best)
					differ = true;
				if (rank > best)
					best = rank;

				ranks[*it] = rank;
			}

			if (ranks.empty() || !differ)
				continue;

			for (std::map<int, long>::const_iterator it = ranks.begin(); it != ranks.end(); ++it)
				cpuClass[it->first] = it->second == best ? 1 : 0;
			break;
		}
	}

	std::set<PhysicalCoreId> physicalCores;
	std::set<PhysicalCoreId> performancePhysicalCores;
	std::set<PhysicalCoreId> efficiencyPhysicalCores;
	bool haveAllCoreIds = true;

	for (std::set<int>::const_iterator it = cpus.begin(); it != cpus.end(); ++it)
	{
		const int cpu = *it;
		++m_logicalCores;

		const std::map<int, int>::const_iterator classIt = cpuClass.find(cpu);
		const int performanceClass = classIt == cpuClass.end() ? -1 : classIt->second;
		if (performanceClass == 1)
			++m_performanceLogicalCores;
		else if (performanceClass == 0)
			++m_efficiencyLogicalCores;

		long package = 0;
		long core = 0;
		if (!readSysfsLong(cpuPath(cpu, "/topology/physical_package_id"), &package)
			|| !readSysfsLong(cpuPath(cpu, "/topology/core_id"), &core))
		{
			haveAllCoreIds = false;
			continue;
		}

		const PhysicalCoreId id = { (int)package, (int)core };
		physicalCores.insert(id);
		if (performanceClass == 1)
			performancePhysicalCores.insert(id);
		else if (performanceClass == 0)
			efficiencyPhysicalCores.insert(id);
	}

	if (m_logicalCores <= 0)
		return false;

	if (haveAllCoreIds)
	{
		m_physicalCores = (int)physicalCores.size();
		m_performancePhysicalCores = (int)performancePhysicalCores.size();
		m_efficiencyPhysicalCores = (int)efficiencyPhysicalCores.size();
	}

	m_hybrid = m_performanceLogicalCores > 0 && m_efficiencyLogicalCores > 0;

	m_probeName = MARK_UV("Linux sysfs");
	return true;
}

#else

bool CPUTopology::probePlatform()
{
	return false;
}

#endif

void CPUTopology::applyGenericFallback()
{
	m_logicalCores = (int)std::thread::hardware_concurrency();
	m_physicalCores = 0;
	m_performanceLogicalCores = 0;
	m_performancePhysicalCores = 0;
	m_efficiencyLogicalCores = 0;
	m_efficiencyPhysicalCores = 0;
	m_hybrid = false;
	m_genericFallback = true;
	m_probeName = MARK_UV("generic");
}

void CPUTopology::deriveCounts()
{
	if (m_logicalCores < 1)
		m_logicalCores = 1;
	if (m_physicalCores < 0 || m_physicalCores > m_logicalCores)
		m_physicalCores = 0;

	// A probe that only half-succeeded is worse than no classification at all,
	// so demand that the classes account for exactly the cores we counted.
	if (m_hybrid
		&& (m_performanceLogicalCores <= 0 || m_efficiencyLogicalCores <= 0
			|| m_performanceLogicalCores + m_efficiencyLogicalCores != m_logicalCores))
		m_hybrid = false;

	if (!m_hybrid)
	{
		m_performanceLogicalCores = 0;
		m_performancePhysicalCores = 0;
		m_efficiencyLogicalCores = 0;
		m_efficiencyPhysicalCores = 0;
	}
	else if (m_physicalCores == 0 || m_performancePhysicalCores + m_efficiencyPhysicalCores != m_physicalCores)
	{
		m_performancePhysicalCores = 0;
		m_efficiencyPhysicalCores = 0;
	}
}

int CPUTopology::performanceThreadPoolSize() const
{
	const int workers = m_logicalCores - 1;
	return workers < 1 ? 1 : workers;
}

int CPUTopology::efficiencyThreadPoolSize() const
{
	const int workers = m_hybrid ? m_efficiencyLogicalCores - 1 : m_logicalCores / 2;
	return workers < 1 ? 1 : workers;
}

UVString CPUTopology::description() const
{
	UVOStringStream stream;

	stream << MARK_UV("probe: ") << m_probeName;
	if (m_genericFallback)
		stream << MARK_UV(" (no platform-specific probe succeeded)");
	stream << MARK_UV("\n");

	stream << MARK_UV("logical cores: ") << m_logicalCores << MARK_UV("\n");

	stream << MARK_UV("physical cores: ");
	if (m_physicalCores > 0)
		stream << m_physicalCores;
	else
		stream << MARK_UV("unknown");
	stream << MARK_UV("\n");

	stream << MARK_UV("hybrid: ") << (m_hybrid ? MARK_UV("yes") : MARK_UV("no")) << MARK_UV("\n");

	if (m_hybrid)
	{
		stream << MARK_UV("performance cores: ") << m_performanceLogicalCores << MARK_UV(" logical, ");
		if (m_performancePhysicalCores > 0)
			stream << m_performancePhysicalCores << MARK_UV(" physical");
		else
			stream << MARK_UV("unknown physical");
		stream << MARK_UV("\n");

		stream << MARK_UV("efficiency cores: ") << m_efficiencyLogicalCores << MARK_UV(" logical, ");
		if (m_efficiencyPhysicalCores > 0)
			stream << m_efficiencyPhysicalCores << MARK_UV(" physical");
		else
			stream << MARK_UV("unknown physical");
		stream << MARK_UV("\n");
	}

	stream << MARK_UV("SMT: ");
	if (m_physicalCores > 0)
		stream << (hasSMT() ? MARK_UV("yes") : MARK_UV("no"));
	else
		stream << MARK_UV("unknown");
	stream << MARK_UV("\n");

	stream << MARK_UV("performance thread pool size: ") << performanceThreadPoolSize() << MARK_UV("\n");
	stream << MARK_UV("efficiency thread pool size: ") << efficiencyThreadPoolSize() << MARK_UV("\n");

	return stream.str();
}
