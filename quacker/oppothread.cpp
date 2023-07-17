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

#include <QtGui>

#include <computerplayer.h>
#include <uv.h>

#include "oppothread.h"

using namespace std;

QuackerDispatch::QuackerDispatch(QObject *parent)
	: QObject(parent), m_shouldAbort(false)
{
}

QuackerDispatch::~QuackerDispatch()
{
}

void QuackerDispatch::signalFractionDone(double fraction)
{
	emit fractionDone(fraction);
}

OppoThread::OppoThread(QObject *parent)
	: QThread(parent), m_player(0)
{
	m_dispatch = new QuackerDispatch(this);
	connect(m_dispatch, SIGNAL(fractionDone(double)), this, SLOT(signalFractionDone(double)));
}

OppoThread::~OppoThread()
{
	m_dispatch->setShouldAbort(true);

	wait();
}

void OppoThread::run()
{
	if (!m_player)
	{
		UVcout << "No computer player for oppo thread to use in position!" << endl;
		return;
	}

	m_player->setPosition(m_position);
	m_player->setDispatch(m_dispatch);
	m_moves = m_player->moves(m_nmoves);
}

void OppoThread::signalFractionDone(double fraction)
{
	emit fractionDone(fraction, this);
}

void OppoThread::setPosition(const Quackle::GamePosition &position)
{
	if (isRunning())
		return;

	m_position = position;
}

void OppoThread::setPlayer(Quackle::ComputerPlayer *player)
{
	if (isRunning())
		return;
	
	m_player = player;
}

void OppoThread::findBestMoves(int nmoves)
{
	if (isRunning())
	{
		UVcout << "OppoThread is already running!" << endl;
		return;
	}

	m_dispatch->setShouldAbort(false);
	m_nmoves = nmoves;
	start(HighPriority);
}

const Quackle::MoveList &OppoThread::moves() const
{
	return m_moves;
}

void OppoThread::abort()
{
	m_dispatch->setShouldAbort(true);
}

