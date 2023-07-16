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

#include <QApplication>
#include <QFileOpenEvent>

#include "quacker.h"

class QuackerApplication : public QApplication
{
public:
	QuackerApplication(int& argc, char** argv)
		: QApplication(argc, argv)
		, m_TopLevel(NULL)
	{
		// empty
	};

	virtual bool event(QEvent* event)
	{
		switch(event->type())
		{
			case QEvent::FileOpen:
			{
				QFileOpenEvent* fileOpenEvent = static_cast<QFileOpenEvent*>(event);
				if (m_TopLevel && !fileOpenEvent->file().isEmpty())
				{
					m_TopLevel->openFile(fileOpenEvent->file());
					return true;
				}
			}
			// no break
			default:
				return QApplication::event(event);
		}
	}

	void setTopLevel(TopLevel* topLevel)
	{
		m_TopLevel = topLevel;
	}

private:
	TopLevel* m_TopLevel;
};

int main(int argc, char **argv)
{
#if (QT_VERSION >= QT_VERSION_CHECK(5, 6, 0) && QT_VERSION < QT_VERSION_CHECK(6, 0, 0))
	QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
#endif
	QuackerApplication a(argc, argv);
	TopLevel topLevel;
	a.setTopLevel(&topLevel);
	topLevel.show();
	return a.exec();
}

