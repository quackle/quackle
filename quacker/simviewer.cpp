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

#include <iostream>

#include <QtWidgets>

#include <quackleio/util.h>

#include "simviewer.h"

SimViewer::SimViewer(QWidget *parent)
	: QDialog(parent)
{
	m_tabs = new QTabWidget;

	m_averagesTab = new AveragesTab;
	m_tabs->addTab(m_averagesTab, tr("&Averages"));

	QPushButton *closeButton = new QPushButton(tr("&Close"));
	closeButton->setDefault(true);

	connect(closeButton, SIGNAL(clicked()), this, SLOT(accept()));

	QHBoxLayout *buttonLayout = new QHBoxLayout;
	buttonLayout->addStretch(1);
	buttonLayout->addWidget(closeButton);

	QVBoxLayout *topLayout = new QVBoxLayout;
	topLayout->addWidget(m_tabs);
	topLayout->addLayout(buttonLayout);
	setLayout(topLayout);

	setWindowTitle(tr("Simulation Results - Quackle"));
}

void SimViewer::setSimulator(const Quackle::Simulator &simulator)
{
	m_averagesTab->setSimulator(simulator);
	setWindowTitle(tr("%1 iterations of %2 - Quackle").arg(simulator.iterations()).arg(QuackleIO::Util::letterStringToQString(simulator.currentPosition().currentPlayer().rack().tiles())));
}

/////////////

AveragesTab::AveragesTab(QWidget *parent)
	: QWidget(parent)
{
	QVBoxLayout *topLayout = new QVBoxLayout(this);

	m_textEdit = new QTextEdit;
	m_textEdit->setReadOnly(true);

	QPushButton *explainButton = new QPushButton(tr("&Explain me!"));
	connect(explainButton, SIGNAL(clicked()), this, SLOT(explain()));

	topLayout->addWidget(m_textEdit);
	//topLayout->addWidget(explainButton);
}

void AveragesTab::setSimulator(const Quackle::Simulator &simulator)
{
	QString html;

	html += statisticTable(simulator);

	html += "<hr />";

	for (const auto& it : simulator.simmedMoves())
	{
		if (!it.includeInSimulation())
			continue;

		QString levels;
		for (const auto& levelIt : it.levels)
		{
			QString plays;
			for (const auto& valueIt : levelIt.statistics)
			{
				//plays += QString("(%1) ").arg((*valueIt).score.averagedValue());
				//plays += tr("(bingos %1) ").arg((*valueIt).bingos.averagedValue());
			}

			if (!plays.isEmpty())
				levels += QString("<li>%1</li>").arg(plays);
		}

		html += QString("<h3>%1</h3><ol>%2</ol>").arg(QuackleIO::Util::moveToDetailedString(it.move)).arg(levels);

		html += "<ul>";
		if (it.residual.hasValues())
			html += tr("<li>Rack leftover value: %1</li>").arg(it.residual.averagedValue());
		if (it.gameSpread.hasValues())
			html += tr("<li>Spread: %1 (sd %2)</li>").arg(it.gameSpread.averagedValue()).arg(it.gameSpread.standardDeviation());
		html += tr("<li>Valuation: %1</li>").arg(it.calculateEquity());
		html += tr("<li>Bogowin %: %1%</li>").arg(it.calculateWinPercentage());
		html += "</ul>";
	}

	// TODO don't scroll to top when resetting
	m_textEdit->setHtml(html);
}

QString AveragesTab::statisticTable(const Quackle::Simulator &simulator)
{
	QString ret;
	for (int levelIndex = 0; levelIndex < simulator.numLevels(); ++levelIndex)
	{
		for (int playerIndex = 0; playerIndex < simulator.numPlayersAtLevel(levelIndex); ++playerIndex)
		{
			if (levelIndex == 0 && playerIndex == 0)
				continue;

			const Quackle::SimmedMoveList::const_iterator end(simulator.simmedMoves().end());
			
			// Little bit of fudgery so that the turn after our next turn is #2,
			// and turn after oppo's next turn is also #2.
			const int turnNumber = levelIndex + (playerIndex == 0? 0 : 1);
			QString name = tr("%1 turn #%2").arg(playerIndex == 0? "Our" : "Oppo").arg(turnNumber);
			if (playerIndex == 1 && levelIndex == 0)
			{
				name = tr("Oppo next turn");
			}
			else if (playerIndex == 0 && levelIndex == 1)
			{
				name = tr("Our next turn");
			}
			ret += QString("<h2>%1</h2>").arg(name);
			
			ret += "<table border=0 cellspacing=4>";
			ret += tr("<tr><th>Candidate</th><th>Score</th><th>Std. Dev.</th><th>Bingo %</th></tr>");
			for (Quackle::SimmedMoveList::const_iterator it = simulator.simmedMoves().begin(); it != end; ++it)
			{
				if (!(*it).includeInSimulation())
					continue;

				ret += "<tr>";
				ret += tr("<td>%1</td>").arg(QuackleIO::Util::moveToDetailedString((*it).move));

				Quackle::AveragedValue value = (*it).getPositionStatistics(levelIndex, playerIndex).getStatistic(Quackle::PositionStatistics::StatisticScore);
				Quackle::AveragedValue bingos = (*it).getPositionStatistics(levelIndex, playerIndex).getStatistic(Quackle::PositionStatistics::StatisticBingos);

				ret += QString("<td>%1</td><td>%2</td>").arg(value.averagedValue()).arg(value.standardDeviation());
				ret += QString("<td>%1</td>").arg(bingos.averagedValue() * 100.0);
				ret += "</tr>";
			}
			ret += "</table>";
		}
	}

	return ret;
}

void AveragesTab::explain()
{
	QMessageBox::information(this, tr("Simulation Details Explanation - Quackle"), tr("<p>A Quackle 2-ply simulation first puts our candidate play on the board, then has opponents make their best plays based on static evaluation, and has us make our best response based on static evaluation. So</p><ol><li>(28 [sd 0]) (39.1503 [sd 17.8229])</li><li>(45.1284 [sd 21.0234])</li></ol><p>means we're looking at a candidate play scoring 28. The average oppo response scored 39.2 with standard deviation 17.8. Our average riposte scored 45.1 with standard deviation 21.0.</p><ul><li>The residual value is the average leave value of our rack at the end of simulation minus the summed average leave value of oppo's racks.</li><li>The spread is the average differential between our score and the leading player's score at the end of an iteration.</li><li>The fake win percentage is how often we had a positive spread at the end of the iteration.</li></ul><p>Note that odd-plied simulations are fun to try occasionally. For example, a 3-ply simulation has two of our plays (including the candidate) and two of each oppo's plays. Also try the <i>Oppos pass</i> option, which has oppos pass for all of their turns in the simulation.</p>"));
}

QSize SimViewer::sizeHint() const
{
	return QSize(400, 400);
}

