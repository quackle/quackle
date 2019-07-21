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

#include <QtWidgets>

#include <game.h>
#include <quackleio/util.h>

#include "geometry.h"
#include "dashboard.h"

PlayerBrief::PlayerBrief()
	: m_isCurrent(false), m_winnerStatus(Nonwinner)
{
	m_vlayout = new QVBoxLayout(this);
	Geometry::setupFramedLayout(m_vlayout);

	m_name = new QLabel;
	m_score = new QLabel;

	m_name->setAlignment(Qt::AlignVCenter | Qt::AlignHCenter);
	m_score->setAlignment(Qt::AlignVCenter | Qt::AlignHCenter);

	m_vlayout->addWidget(m_name);
	m_vlayout->addWidget(m_score);
}

void PlayerBrief::setPlayer(const Quackle::Player &player)
{
	QString nameText = (m_isCurrent? "<b>%1</b>" : "%1");
	nameText = nameText.arg(QuackleIO::Util::uvStringToQString(player.name()));

	if (m_winnerStatus == Winner || m_winnerStatus == Cowinner)
	{
		nameText += QString("<h3>%1</h3>").arg(m_winnerStatus == Winner? tr("Winner!") : tr("Cowinner"));
	}

	m_name->setText(nameText);
	m_score->setText(QString("<h2>%1</h2>").arg(player.score()));

	if (m_isCurrent)
	{
		setFrameStyle(QFrame::Panel | QFrame::Sunken);
		setLineWidth(2);
	}
	else
	{
		setFrameStyle(QFrame::NoFrame);
	}

}

////////////

Dashboard::Dashboard(QWidget *parent)
	: HistoryView(parent)
{
	m_hlayout = new QHBoxLayout(this);
	Geometry::setupInnerLayout(m_hlayout);
}

Dashboard::~Dashboard()
{
}

void Dashboard::historyChanged(const Quackle::History &history)
{
	const Quackle::PlayerList players(history.currentPosition().endgameAdjustedScores());
	const size_t numberOfPlayers = players.size();
	const bool gameOver = history.currentPosition().gameOver();

	while (m_briefs.size() > numberOfPlayers)
	{
		delete m_briefs.back();
		m_briefs.pop_back();
	}

	while (m_briefs.size() < numberOfPlayers)
	{
		m_briefs.push_back(new PlayerBrief);
		m_hlayout->addWidget(m_briefs.back());
		m_briefs.back()->show();
	}

	Quackle::PlayerList winners;
	if (gameOver)
		winners = history.currentPosition().leadingPlayers();

	int playerCountFromZero = 0;
	QList<PlayerBrief *>::iterator briefIt = m_briefs.begin();
	for (Quackle::PlayerList::const_iterator playerIt = players.begin(); playerIt != players.end(); ++playerIt, ++briefIt, ++playerCountFromZero)
	{
		const bool isCurrentPlayer = !gameOver && *playerIt == history.currentPosition().playerOnTurn();

		(*briefIt)->setCurrentPlayer(isCurrentPlayer);
		(*briefIt)->setWinnerStatus(Nonwinner);

		if (gameOver)
		{
			Quackle::PlayerList::const_iterator winnersIt;
			for (winnersIt = winners.begin(); winnersIt != winners.end(); ++winnersIt)
			{
				if (*playerIt == *winnersIt)
				{
					(*briefIt)->setWinnerStatus(winners.size() > 1? Cowinner : Winner);
					break;
				}
			}
		}

		(*briefIt)->setPlayer(*playerIt);
	}
}

