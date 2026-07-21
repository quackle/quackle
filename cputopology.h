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

#ifndef QUACKLE_CPUTOPOLOGY_H
#define QUACKLE_CPUTOPOLOGY_H

#include "uv.h"

namespace Quackle
{

// What we know about the CPUs of the machine we're running on, and what that
// implies about how big a thread pool ought to be.
//
// Everything is probed once, at construction; the object is immutable
// afterwards. Platform-specific probes exist for macOS, Windows, and Linux
// (identical source for ARM64 and x86-64 on each); anywhere else, or when a
// probe fails, a generic fallback based on std::thread::hardware_concurrency()
// fills in the logical core count and leaves the rest unknown.

class CPUTopology
{
public:
	// Probes the machine. Cheap; nothing here is lazy.
	CPUTopology();

	// All schedulable CPUs, counting SMT siblings separately. Always at least 1.
	int logicalCoreCount() const;

	// Distinct physical cores; SMT siblings collapse to one. 0 if unknown.
	int physicalCoreCount() const;

	// Core counts by performance class. All 0 unless isHybrid() is true.
	int performanceLogicalCoreCount() const;
	int performancePhysicalCoreCount() const;
	int efficiencyLogicalCoreCount() const;
	int efficiencyPhysicalCoreCount() const;

	// True if the machine has distinct performance and efficiency cores and we
	// were able to tell them apart.
	bool isHybrid() const;

	// True if some core runs more than one logical CPU (hyperthreading etc.).
	bool hasSMT() const;

	// True if no platform-specific probe was available, or the one we tried
	// failed. Everything but logicalCoreCount() is then unknown.
	bool usedGenericFallback() const;

	// Which probe produced these numbers, for diagnostics.
	const UVString &probeName() const;

	// Worker threads for a throughput-oriented pool: every logical core, less
	// one reserved for the main thread, which coordinates rather than working.
	// Never less than 1.
	int performanceThreadPoolSize() const;

	// Worker threads for background or low-priority work: on a hybrid machine
	// the efficiency cores less one for the main thread, otherwise half the
	// logical cores. Never less than 1.
	int efficiencyThreadPoolSize() const;

	// Multi-line human-readable dump of everything above. Unknown quantities
	// are reported as "unknown" rather than as zero.
	UVString description() const;

private:
	// Fills in the members from the platform. Returns false if it couldn't.
	bool probePlatform();

	// Last-resort probe; sets m_genericFallback.
	void applyGenericFallback();

	// Sanity-checks and normalizes whatever a probe left behind.
	void deriveCounts();

	int m_logicalCores;
	int m_physicalCores;
	int m_performanceLogicalCores;
	int m_performancePhysicalCores;
	int m_efficiencyLogicalCores;
	int m_efficiencyPhysicalCores;
	bool m_hybrid;
	bool m_genericFallback;
	UVString m_probeName;
};

inline int CPUTopology::logicalCoreCount() const
{
	return m_logicalCores;
}

inline int CPUTopology::physicalCoreCount() const
{
	return m_physicalCores;
}

inline int CPUTopology::performanceLogicalCoreCount() const
{
	return m_performanceLogicalCores;
}

inline int CPUTopology::performancePhysicalCoreCount() const
{
	return m_performancePhysicalCores;
}

inline int CPUTopology::efficiencyLogicalCoreCount() const
{
	return m_efficiencyLogicalCores;
}

inline int CPUTopology::efficiencyPhysicalCoreCount() const
{
	return m_efficiencyPhysicalCores;
}

inline bool CPUTopology::isHybrid() const
{
	return m_hybrid;
}

inline bool CPUTopology::hasSMT() const
{
	return m_physicalCores > 0 && m_logicalCores > m_physicalCores;
}

inline bool CPUTopology::usedGenericFallback() const
{
	return m_genericFallback;
}

inline const UVString &CPUTopology::probeName() const
{
	return m_probeName;
}

}

#endif
