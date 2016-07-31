/*
 *  Quackle -- Crossword game artificial intelligence and analysis tool
 *  Copyright (C) 2005-2016 Jason Katz-Brown and John O'Laughlin.
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


#include <iostream>
#include <string>
#include <vector>

#include <QFile>
#include <QSettings>
#include <QString>
#include <QTextStream>

using namespace std;

#include "boardparameters.h" 
#include "computerplayer.h"
#include "datamanager.h"
#include "lexiconparameters.h"
#include "strategyparameters.h"
#include "quacker/customqsettings.h"
#include "quackleio/graphicalreporter.h"
#include "quackleio/logania.h"
#include "quackleio/queenie.h"
#include "quackleio/util.h"

class Command;

class CommandFlag
{
public:
	CommandFlag(const string& flagName)
		: m_name(flagName)
	{};

	const string& name() const { return m_name; };

private:
	string m_name;
};

class CommandDispatcher
{
public:
	virtual int dispatch(const Command& command) = 0;
};

class Command
{
public:
	enum FileArgumentType {
		eFileArgumentNone,
		eFileArgumentFile,
		eFileArgumentMultipleFiles,
		eFileArgumentGlob // implies multiple files
	};

	Command(const string& commandName)
		: m_commandName(commandName)
	{};

	Command& addFlag(const string& flagName) { m_flags.emplace_back(CommandFlag{flagName}); return *this; };
	Command& addFileArgument(bool multipleFiles, bool globFiles, const string& defaultExtension)
	{
		m_fileArgument = globFiles ? eFileArgumentGlob : (multipleFiles ? eFileArgumentMultipleFiles : eFileArgumentFile);
		m_defaultExtension = defaultExtension;
		return *this;
	};
	Command& addHelp(const string& help) { m_help = help; return *this; };
	Command& addDispatcher(CommandDispatcher* dispatcher) { m_dispatcher = dispatcher; return *this; };

	int dispatch(int argc, char* argv[]) { parseArgs(argc, argv); return m_dispatcher->dispatch(*this); };
	const string& name() const { return m_commandName; };
	const string& defaultExtension() const { return m_defaultExtension; };
	const string& help() const { return m_help; };
	const vector<CommandFlag>& flags() const { return m_flags; };
	FileArgumentType fileArgument() const { return m_fileArgument; };
	const vector<string>& fileNames() const { return m_fileNames; };

private:
	string m_commandName;
	string m_help;
	string m_defaultExtension;
	CommandDispatcher* m_dispatcher = NULL;
	vector<CommandFlag> m_flags;
	FileArgumentType m_fileArgument = eFileArgumentNone;
	vector<string> m_fileNames;

	void parseArgs(int argc, char* argv[])
	{
		for (int i = 2; i < argc; i++)
			m_fileNames.emplace_back(string(argv[i]));
	}
};

class CommandParser
{
public:
	Command& addCommand(string cmd) { m_commands.emplace_back(Command{cmd}); return m_commands.back(); };

	int dispatch(int argc, char* argv[])
	{
		for (auto& command : m_commands)
		{
			if (argc > 1 && command.name() == argv[1])
				return command.dispatch(argc, argv);
		}
		for (auto& command : m_commands)
		{
			if (command.name() == "help")
				return command.dispatch(argc, argv);
		}
		return 0;
	}

	const vector<Command>& commands() const { return m_commands; };

private:
	vector<Command> m_commands;
};

class QuackleInitializer
{
protected:
	void initQuackle()
	{
		CustomQSettings settings;
		QString boardName = settings.value("quackle/settings/board-name", QString("")).toString();
		settings.beginGroup("quackle/boardparameters");

		if (boardName.isEmpty() || !settings.contains(boardName))
		{
			cout << "Could not find configured board.  Using a generic board which is probably wrong.\n";
			m_dataManager.setBoardParameters(new Quackle::EnglishBoard());
		}
		else
		{
			QByteArray boardParameterBytes = qUncompress(settings.value(boardName).toByteArray());
			string boardParameterBuf;
			boardParameterBuf.assign((const char *) boardParameterBytes, boardParameterBytes.size());
			istringstream boardParameterStream(boardParameterBuf);
			m_dataManager.setBoardParameters(Quackle::BoardParameters::Deserialize(boardParameterStream));
		}

		string dataDir;
		if (QFile::exists("data"))
			dataDir = "data";
		else if (QFile::exists("../data"))
			dataDir = "../data";
		else if (QFile::exists("../../data"))
			dataDir = "../../data";
		else if (QFile::exists("Quackle.app/Contents/data"))
			dataDir = "Quackle.app/Contents/data";
		else
		{
			cout << "Error Initializing Data Files";
			dataDir = "data";
		}
		m_dataManager.setAppDataDirectory(dataDir);
		m_dataManager.lexiconParameters()->loadDawg(Quackle::LexiconParameters::findDictionaryFile("csw15.dawg"));
		m_dataManager.lexiconParameters()->loadGaddag(Quackle::LexiconParameters::findDictionaryFile("csw15.gaddag"));
		m_dataManager.strategyParameters()->initialize("csw15");
	}

	static Quackle::DataManager m_dataManager;
	static QuackleIO::UtilSettings m_utilSettings;
};

Quackle::DataManager QuackleInitializer::m_dataManager;
QuackleIO::UtilSettings QuackleInitializer::m_utilSettings;

class ReportCommand : public CommandDispatcher, public QuackleInitializer
{
public:
	virtual int dispatch(const Command& command)
	{
		initQuackle();
		for (const auto& fileName : command.fileNames())
		{
			string extension = fileName.substr(fileName.size() - 4);
			if (extension == ".gcg" || extension == ".GCG")
			{
				QString qFileName = QString::fromStdString(fileName);
				string output = fileName.substr(0, fileName.size() - 4) + ".html";
				GraphicalReporter reporter(output);
				QuackleIO::Logania *logania = QuackleIO::Queenie::self()->loganiaForFile(qFileName);
				Quackle::Game* game = logania->read(qFileName, QuackleIO::Logania::MaintainBoardPreparation);

				if (command.name() == "report")
				{
					Quackle::StaticPlayer player;
					reporter.reportGame(*game, &player);
				}
				else if (command.name() == "export")
					reporter.exportGame(*game);

				delete game;
			}
		}
		return 0;
	}
};

class TestCommand : public CommandDispatcher, public QuackleInitializer
{
public:
	virtual int dispatch(const Command& command)
	{
		initQuackle();
		for (const auto& fileName : command.fileNames())
		{
			string extension = fileName.substr(fileName.size() - 4);
			if (extension == ".gcg" || extension == ".GCG")
			{
				QString qFileName = QString::fromStdString(fileName);
				QFile qFile(qFileName);
				string output = fileName.substr(0, fileName.size() - 4) + ".html";
				GraphicalReporter reporter(output);
				QuackleIO::Logania *logania = QuackleIO::Queenie::self()->loganiaForFile(qFileName);
				Quackle::Game* game = logania->read(qFileName, QuackleIO::Logania::MaintainBoardPreparation);

				QString qOutput;
				QTextStream qOutputStream(&qOutput, QIODevice::WriteOnly);
				logania->write(*game, qOutputStream);
				qOutputStream.flush();

				qFile.open(QIODevice::ReadOnly | QIODevice::Text);
				QTextStream qInputStream(&qFile);
				QString qInput(qInputStream.readAll());

				qInput.replace("\r\n","\n");
				qOutput.replace("\r\n","\n");
				if (qInput != qOutput)
				{
					cout << fileName << " differs\n";
					cout << qInput.toStdString();
					cout << qOutput.toStdString();
				}

				delete game;
			}
		}
		return 0;
	}
};

class StatsCommand : public CommandDispatcher, public QuackleInitializer
{
public:
	virtual int dispatch(const Command& command)
	{
		initQuackle();
		for (const auto& input : command.fileNames())
		{
			string extension = input.substr(input.size() - 4);
			if (extension == ".gcg" || extension == ".GCG")
			{
				QString qInput = QString::fromStdString(input);
				QuackleIO::Logania *logania = QuackleIO::Queenie::self()->loganiaForFile(qInput);
				Quackle::Game* game = logania->read(qInput, QuackleIO::Logania::MaintainBoardPreparation);

				delete game;
			}
		}
		return 0;
	}
};

class HelpCommand : public CommandDispatcher
{
public:
	HelpCommand(const CommandParser& options)
		: m_options(options)
	{
	};

	virtual int dispatch(const Command& command)
	{
		cout << "usage: quack <command> [<args>]\n\nThe commands are:\n\n";
		for (const auto& command : m_options.commands())
		{
			cout << command.name();
			for (const auto& flag : command.flags())
				cout << " [--" << flag.name() << "]";
			if (command.fileArgument() != Command::eFileArgumentNone)
			{
				cout << " <file" << command.defaultExtension() << ">";
				if (command.fileArgument() != Command::eFileArgumentFile)
					cout << " [<file" << command.defaultExtension() << "> ...]";
			}
			cout << "\n  " << command.help() << "\n";
		}
		return 0;
	}

private:
	const CommandParser& m_options;
};

int main(int argc, char* argv[])
{
	CommandParser options;

	options.addCommand("export")
			.addFileArgument(true, true, ".gcg")
			.addHelp("Run an HTML report on the GCG file")
			.addDispatcher(new ReportCommand());

	options.addCommand("report")
			.addFileArgument(true, true, ".gcg")
			.addHelp("Run an HTML report on the GCG file")
			.addDispatcher(new ReportCommand());

	options.addCommand("stats")
			.addFileArgument(true, true, ".gcg")
			.addHelp("Report aggregate statistics on a collection of GCG files")
			.addDispatcher(new StatsCommand());

	options.addCommand("test")
			.addFileArgument(true, true, ".gcg")
			.addHelp("Test GCG round-trip")
			.addDispatcher(new TestCommand());

	options.addCommand("help")
			.addHelp("Show this message")
			.addDispatcher(new HelpCommand(options));

	return options.dispatch(argc, argv);
}
