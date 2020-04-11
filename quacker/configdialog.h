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

#ifndef QUACKER_CONFIG_DIALOG_H
#define QUACKER_CONFIG_DIALOG_H

#include <QDialog>

class QListWidget;
class QListWidgetItem;
class QStackedWidget;

class ConfigPage : public QWidget
{
Q_OBJECT

public:
	ConfigPage(QWidget *parent = 0);

	virtual void readConfig() = 0;
	virtual void writeConfig() = 0;

	QString pageTitle() const { return m_pageTitle; };

protected:
	QString m_pageTitle;
};

class ConfigDialog : public QDialog
{
Q_OBJECT

public:
	ConfigDialog();

signals:
	void refreshViews();

public slots:
	void changePage(QListWidgetItem *current, QListWidgetItem *previous);
	void apply();
	void submit();

private:
	QListWidget *m_contentsWidget;
	QStackedWidget *m_pagesWidget;
	QList<ConfigPage *> m_configPages;
};

#endif

