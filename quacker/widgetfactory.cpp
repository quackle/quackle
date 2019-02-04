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

#include "bagdisplay.h"
#include "boarddisplay.h"
#include "graphicalboard.h"
#include "boardsetup.h"
#include "rackdisplay.h"
#include "widgetfactory.h"

View *TextFactory::createBoardDisplay()
{
	return new TextBoard;
}

View *TextFactory::createRackDisplay()
{
	return new QuickEntryRack;
}

View *TextFactory::createBagDisplay()
{
	return new BagDisplay;
}

View *GraphicalFactory::createBoardDisplay()
{
	return new GraphicalBoard;
}

View *GraphicalFactory::createRackDisplay()
{
	return new QuickEntryRack;
}

View *GraphicalFactory::createBagDisplay()
{
	return new BagDisplay;
}

View *BoardSetupFactory::createBoardDisplay()
{
	return new BoardSetup;
}

View *BoardSetupFactory::createRackDisplay()
{
	return new View;
}

View *BoardSetupFactory::createBagDisplay()
{
	return new View;
}

