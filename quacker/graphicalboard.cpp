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
#include <math.h>

#include <QtGui>
#include <QVBoxLayout>
#include <qdrawutil.h>

#include <game.h>
#include <move.h>
#include <boardparameters.h>
#include <quackleio/util.h>

#include "geometry.h"
#include "graphicalboard.h"
#include "quackersettings.h"
#include "settings.h"

using namespace std;

const double GraphicalBoardFrame::s_markOtherLengthMultiplier = 0.6;

const double TileWidget::s_defaultLetterScale = 0.7;

GraphicalBoard::GraphicalBoard(QWidget *parent)
    : BoardWithQuickEntry(parent)
{
    m_boardFrame = new GraphicalBoardFrame;
    connect(m_boardFrame, SIGNAL(localCandidateChanged(const Quackle::Move *)), this, SLOT(setLocalCandidate(const Quackle::Move *)));

    m_boardWrapper = new QWidget;

    QVBoxLayout *helperLayout = new QVBoxLayout(m_boardWrapper);
    Geometry::setupInnerLayout(helperLayout);

    m_vlayout->addWidget(m_boardWrapper);
    helperLayout->addWidget(m_boardFrame);
    m_vlayout->setStretchFactor(m_boardWrapper, 10);

    m_subviews.push_back(m_boardFrame);
    connectSubviewSignals();
}

GraphicalBoard::~GraphicalBoard()
{
}

void GraphicalBoard::expandToFullWidth()
{
    m_boardFrame->expandToSize(m_boardWrapper->size());
}

void GraphicalBoard::resizeEvent(QResizeEvent * /* event */)
{
    QTimer::singleShot(0, this, SLOT(expandToFullWidth()));
}

///////////////////

GraphicalBoardFrame::GraphicalBoardFrame(QWidget *parent)
    : View(parent), m_ignoreRack(false), m_alwaysShowVerboseLabels(false), m_boardSize(0, 0), m_sideLength(0)
{
    setFrameStyle(QFrame::StyledPanel | QFrame::Raised);
    setLineWidth(2);

    setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum);
    setFocusPolicy(Qt::StrongFocus);

    QColor color(PixmapCacher::self()->markColor);
    QPalette customPalette;
    customPalette.setColor(QPalette::Light, color.lighter(s_highlightFactor));
    customPalette.setColor(QPalette::Mid, color);
    customPalette.setColor(QPalette::Dark, color.darker(130));
    setPalette(customPalette);

    PixmapCacher::self()->tileFont = font();
#if defined(Q_OS_WIN)
    PixmapCacher::self()->tileFont.setFamily(QString("Arial"));
#endif // Q_OS_WIN

    //expandToSize(QSize(15 * 25, 15 * 25));

    m_board.prepareEmptyBoard();
    m_candidate = Quackle::Move::createNonmove();
    resetArrow();
}

GraphicalBoardFrame::~GraphicalBoardFrame()
{
    PixmapCacher::cleanUp();
}

void GraphicalBoardFrame::staticDrawPosition(const Quackle::GamePosition &position, const QSize &size, QPixmap *pixmap)
{
    GraphicalBoardFrame doozy;
    doozy.positionChanged(&position);
    doozy.expandToSize(size);
    doozy.generateBoardPixmap(pixmap);
}

void GraphicalBoardFrame::positionChanged(const Quackle::GamePosition *position)
{
    m_board = position->board();
    m_rack = position->currentPlayer().rack();
    m_ignoreRack = !position->currentPlayer().racksAreKnown();

    m_board.updateBritishness();

    resetArrow();
    m_candidate = position->moveMade();

    prepare();
}

void GraphicalBoardFrame::prepare()
{
    drawBoard(m_board);
    drawMove(m_candidate);

    if (isOnBoard(m_arrowRoot))
        drawArrow(m_arrowRoot, m_arrowDirection);

    generateBoardPixmap(&m_pixmap);
    update();
}

void GraphicalBoardFrame::expandToSize(const QSize &maxSize)
{
    m_maxSize = maxSize;

    // if empty we delay this call
    if (m_boardSize.isEmpty())
        return;
    else
    {
        // TODO we get here too often while resizing

        // do calculations in terms of one axis, arbitrarily, width
        QSize lastCandidate;
        QSize candidate(0, 0);
        do
        {
            lastCandidate = candidate;
            candidate += m_boardSize;
        }
        while (candidate.width() < m_maxSize.width() && candidate.height() < m_maxSize.height());

        const int maxWidth = lastCandidate.width();

        const double numBlocksWidthwise = ((double)m_boardSize.width() + s_markOtherLengthMultiplier);

        m_sideLength = (int)floor((maxWidth - frameWidth() * 2) / numBlocksWidthwise);

        if (m_sideLength < 0)
            m_sideLength = 0;

        resizeWidgets(m_sideLength);

        const int shorterMarkWidth = markAt(QSize(0, 0))->size().width();
        const int shorterMarkHeight = markAt(QSize(0, 0))->size().height();
        m_tilesOffset.setX(shorterMarkWidth);
        m_tilesOffset.setY(shorterMarkHeight);

        m_sizeForBoard.setWidth(shorterMarkWidth + m_boardSize.width() * m_sideLength);
        m_sizeForBoard.setHeight(shorterMarkHeight + m_boardSize.height() * m_sideLength);
        setMaximumSize(m_sizeForBoard + QSize(frameWidth() * 2, frameWidth() * 2));

        prepare();
    }
}

void GraphicalBoardFrame::generateBoardPixmap(QPixmap *pixmap)
{
    if (m_sizeForBoard.isEmpty())
    {
        *pixmap = QPixmap(0, 0);
        return;
    }

    *pixmap = QPixmap(m_sizeForBoard * devicePixelRatio());
    pixmap->setDevicePixelRatio(devicePixelRatio());
    QPainter painter(pixmap);

    for (QSize currentTile(0, 0); currentTile.height() < m_boardSize.height(); currentTile.setHeight(currentTile.height() + 1))
    {
        for (currentTile.setWidth(0); currentTile.width() < m_boardSize.width(); currentTile.setWidth(currentTile.width() + 1))
        {
            TileWidget *tile = tileAt(currentTile);
            if (!tile)
                continue;

            painter.drawPixmap(coordinatesOfTile(currentTile), tile->tilePixmap());
        }
    }

    for (int row = 0; row <= m_boardSize.height(); ++row)
    {
        const QSize location(0, row);
        painter.drawPixmap(coordinatesOfMark(location), markAt(location)->tilePixmap());
    }

    for (int col = 1; col <= m_boardSize.width(); ++col)
    {
        const QSize location(col, 0);
        painter.drawPixmap(coordinatesOfMark(location), markAt(location)->tilePixmap());
    }
}

void GraphicalBoardFrame::drawBoard(const Quackle::Board &board)
{
    QSize newBoardSize(board.width(), board.height());

    if (m_boardSize != newBoardSize)
    {
        // if it was empty, we need to recalculate
        // tile widths
        bool wasEmpty = m_boardSize.isEmpty();
        
        deleteWidgets();
        m_boardSize = newBoardSize;
        recreateWidgets();

        if (wasEmpty)
            expandToSize(m_maxSize);
    }

    for (QSize currentTile(0, 0); currentTile.height() < m_boardSize.height(); currentTile.setHeight(currentTile.height() + 1))
    {
        for (currentTile.setWidth(0); currentTile.width() < m_boardSize.width(); currentTile.setWidth(currentTile.width() + 1))
        {
            Quackle::Board::TileInformation info(board.tileInformation(currentTile.height(), currentTile.width()));

            TileWidget *tile = tileAt(currentTile);
            if (!tile)
                continue;

            tile->setInformation(info);

            tile->setArrowDirection(NoArrow);
            tile->setCemented(info.tileType == Quackle::Board::LetterTile);
            tile->prepare();
        }
    }
}

TileWidget *GraphicalBoardFrame::tileAt(const QSize &loc)
{
    QSize mapEntry(loc + QSize(1, 1));
    if (!m_tileWidgets.contains(mapEntry))
    {
        cerr << "error! graphical board can't find a tile" << endl;
        return 0;
    }

    return m_tileWidgets.value(mapEntry);
}

QSize GraphicalBoardFrame::locationForPosition(const QPoint &pos)
{
    QPoint semiRegularized = (pos - QPoint(frameWidth(), frameWidth()) - m_tilesOffset);
    QSize finalSize((int)floor((double)semiRegularized.x() / m_sideLength), (int)floor((double)semiRegularized.y() / m_sideLength));
    return finalSize;
}

TileWidget *GraphicalBoardFrame::markAt(const QSize &loc)
{
    if (!m_tileWidgets.contains(loc))
    {
        cerr << "error! graphical board can't find a mark" << endl;
        return 0;
    }

    return m_tileWidgets.value(loc);
}

void GraphicalBoardFrame::addTile(const QSize &loc, TileWidget *tile)
{
    m_tileWidgets.insert(loc + QSize(1, 1), tile);
}

void GraphicalBoardFrame::removeTile(const QSize &loc)
{
    m_tileWidgets.remove(loc + QSize(1, 1));
}

void GraphicalBoardFrame::addMark(const QSize &loc, MarkWidget *tile)
{
    m_tileWidgets.insert(loc, tile);
}

void GraphicalBoardFrame::removeMark(const QSize &loc)
{
    m_tileWidgets.remove(loc);
}

QPoint GraphicalBoardFrame::coordinatesOfTile(const QSize &loc)
{
    QPoint point(loc.width(), loc.height());
    return (point * m_sideLength) + m_tilesOffset;
}

QPoint GraphicalBoardFrame::coordinatesOfMark(const QSize &loc)
{
    QPoint point(loc.width(), loc.height());

    if (loc == QSize(0, 0))
        return point;
    
    QPoint vector(point.x() == 0? 0 : 1, point.y() == 0? 0 : 1);

    return ((point - vector) * m_sideLength + QPoint(m_tilesOffset.x() * vector.x(), m_tilesOffset.y() * vector.y()));
}

void GraphicalBoardFrame::drawMove(const Quackle::Move &move)
{
    if (move.action == Quackle::Move::Place || move.action == Quackle::Move::PlaceError)
    {
        if (move.tiles().empty())
            return;

        const QSize startTile(move.startcol, move.startrow);

        const Quackle::LetterString::const_iterator end(move.tiles().end());
        int i = 0;
        for (Quackle::LetterString::const_iterator it = move.tiles().begin(); it != end; ++it, ++i)
        {
            // if this is a cemented letter that we shouldn't overwrite
            if (move.isAlreadyOnBoard(*it))
                continue;

            QSize currentTile(startTile);
            if (move.horizontal)
                currentTile.setWidth(currentTile.width() + i);
            else
                currentTile.setHeight(currentTile.height() + i);

            TileWidget *tileWidget = tileAt(currentTile);
            if (!tileWidget)
                continue;

            Quackle::Board::TileInformation info;
            info.tileType = Quackle::Board::LetterTile;

            info.isBlank = QUACKLE_ALPHABET_PARAMETERS->isBlankLetter(*it);
            info.letter = QUACKLE_ALPHABET_PARAMETERS->clearBlankness(*it);

            tileWidget->setInformation(info);
            tileWidget->setCemented(false);
            tileWidget->prepare();
        }
    }
}

void GraphicalBoardFrame::drawArrow(const QSize &location, int arrowDirection)
{
    TileWidget *tile = tileAt(location);
    if (!tile)
        return;
    
    tile->setArrowDirection(arrowDirection);
    tile->prepare();
}

void GraphicalBoardFrame::deleteWidgets()
{
    if (m_boardSize.isEmpty())
        return;

    for (QSize currentTile(0, 0); currentTile.height() < m_boardSize.height(); currentTile.setHeight(currentTile.height() + 1))
    {
        for (currentTile.setWidth(0); currentTile.width() < m_boardSize.width(); currentTile.setWidth(currentTile.width() + 1))
        {
            delete tileAt(currentTile);
            removeTile(currentTile);
        }
    }

    for (int row = 0; row <= m_boardSize.height(); ++row)
    {
        delete markAt(QSize(0, row));
        removeMark(QSize(0, row));
    }

    for (int col = 1; col <= m_boardSize.width(); ++col)
    {
        delete markAt(QSize(col, 0));
        removeMark(QSize(col, 0));
    }
}

void GraphicalBoardFrame::recreateWidgets()
{
    Quackle::Board emptyBoard;
    emptyBoard.prepareEmptyBoard();

    for (QSize currentTile(0, 0); currentTile.height() < m_boardSize.height(); currentTile.setHeight(currentTile.height() + 1))
    {
        for (currentTile.setWidth(0); currentTile.width() < m_boardSize.width(); currentTile.setWidth(currentTile.width() + 1))
        {
            TileWidget *newTile = new TileWidget;

            newTile->setDevicePixelRatio(devicePixelRatio());
            newTile->setLocation(currentTile);
            newTile->setAlwaysShowVerboseLabels(m_alwaysShowVerboseLabels);
            newTile->setOriginalInformation(emptyBoard.tileInformation(currentTile.height(), currentTile.width()));

            addTile(currentTile, newTile);
        }
    }

    for (int row = 0; row <= m_boardSize.height(); ++row)
    {
        MarkWidget *newMark = new MarkWidget;

        newMark->setDevicePixelRatio(devicePixelRatio());
        if (row == 0)
            newMark->setCapstone();
        else
            newMark->setRow(row);

        addMark(QSize(0, row), newMark);
    }

    for (int col = 1; col <= m_boardSize.width(); ++col)
    {
        MarkWidget *newMark = new MarkWidget;

        newMark->setDevicePixelRatio(devicePixelRatio());
        newMark->setCol(col);

        addMark(QSize(col, 0), newMark);
    }
}

void GraphicalBoardFrame::resizeWidgets(int sideLength)
{
    PixmapCacher::self()->invalidate();

    bool firstTile = true;
    for (QSize currentTile(0, 0); currentTile.height() < m_boardSize.height(); currentTile.setHeight(currentTile.height() + 1))
    {
        for (currentTile.setWidth(0); currentTile.width() < m_boardSize.width(); currentTile.setWidth(currentTile.width() + 1))
        {
            TileWidget *tile = tileAt(currentTile);
            if (!tile)
                continue;

            tile->setSideLength(sideLength);

            if (firstTile)
            {
                emit tileFontChanged(tile->actualLetterFont());
                firstTile = false;
            }
        }
    }

    for (int row = 0; row <= m_boardSize.height(); ++row)
    {
        TileWidget *mark = markAt(QSize(0, row));
        mark->setSideLength(sideLength);
    }

    for (int col = 1; col <= m_boardSize.width(); ++col)
    {
        TileWidget *mark = markAt(QSize(col, 0));
        mark->setSideLength(sideLength);
    }
}

void GraphicalBoardFrame::flushPixmapsAndRedraw()
{
    m_boardSize = QSize();
    prepare();
}

void GraphicalBoardFrame::paintEvent(QPaintEvent *event)
{
    QPainter painter(this);
    painter.drawPixmap(contentsRect().topLeft(), m_pixmap);
    painter.end();

    QFrame::paintEvent(event);
}

void GraphicalBoardFrame::mousePressEvent(QMouseEvent *event)
{
    setFocus();

    if (!wantMousePressEvent(event))
    {
        event->ignore();
        return;
    }

    const QPoint pos(event->pos());

    QSize location = locationForPosition(pos);

    if (location.isValid())
        tileClicked(location, event);

    event->accept();
}

void GraphicalBoardFrame::keyPressEvent(QKeyEvent *event)
{
    enum { Backspace = 0, Delete, Submit, Commit, Append } mode;

    switch (event->key())
    {
    case Qt::Key_Backspace:
        mode = Backspace;
        break;

    case Qt::Key_Delete:
        mode = Delete;
        break;

    case Qt::Key_Return:
    case Qt::Key_Enter:
        mode = event->modifiers() & Qt::ShiftModifier || event->modifiers() & Qt::ControlModifier || event->modifiers() & Qt::MetaModifier? Commit : Submit;
        break;

    default:
        mode = Append;
        break;
    }

    if (!hasCandidate())
    {
        event->ignore();
        return;
    }

    switch (mode)
    {

    // these modes need a candidate
    case Submit:
        submitHandler();
        break;
    
    case Commit:
        commitHandler();
        break;
    
    default:
        if (!hasArrow())
        {
            event->ignore();
            return;
        }

        switch (mode)
        {

        // these modes need an arrow
        case Delete:
            deleteHandler();
            break;
        
        case Backspace:
            backspaceHandler();
            break;
        
        case Append:
			if (event->text().isEmpty() ||
				(event->text().at(0) >= QChar(0xa8) && event->text().at(0) <= QChar(0xb8)) || // combining character
				(event->text().at(0) >= QChar(0x2b9) && event->text().at(0) <= QChar(0x2ff))) // combining character
				break; // let AltGr (Ctrl+Alt) and other composite keyboard events slip through
            else if (event->modifiers() & Qt::AltModifier || event->modifiers() & Qt::ControlModifier)
            {
				if (!event->text().isEmpty() &&
					((event->text().at(0) >= 'a' && event->text().at(0) <= 'z') ||
					 (event->text().at(0) >= 'A' && event->text().at(0) <= 'Z')))
				{
					event->ignore();
					return;
				}
            }
    
            appendHandler(event->text(), event->modifiers() & Qt::ShiftModifier);
            break;

        case Commit:
        case Submit:
            break;
        }
        break;
    }

    prepare();

    event->accept();
}

bool GraphicalBoardFrame::wantMousePressEvent(const QMouseEvent *event) const
{
    return (event->button() == Qt::LeftButton);
}

Quackle::Move GraphicalBoardFrame::flip(const Quackle::Move &flippee)
{
    if (!m_board.isConnected(flippee))
        return flippee;

    Quackle::MoveList words = m_board.allWordsFormedBy(flippee);
    Quackle::Move flipped = words.front();
    if (flipped.tiles().length() > 1)
    {
        return flipped;
    }
    else
    {
        return flippee;
    }
}

void GraphicalBoardFrame::prettifyAndSetLocalCandidate(const Quackle::Move &candidate)
{
    m_candidate = candidate;
    m_candidate.setPrettyTiles(m_board.prettyTilesOfMove(m_candidate));

    if (candidate.action == Quackle::Move::Place && candidate.wordTilesWithNoPlayThru().empty())
    {
        emit statusMessage(tr("Click again to switch arrow orientation, or type a word."));
        return;
    }

    //UVcout << "m_candidate: " << m_candidate << endl;

    if (m_candidate.wordTilesWithNoPlayThru().length() == 1) 
    {
        const Quackle::Move &move = flip(m_candidate);
        emit localCandidateChanged(&move);

        emit statusMessage(tr("Press Enter to add %1 to candidate list, or Control+Enter to commit to it immediately.").arg(QuackleIO::Util::moveToDetailedString(flip(m_candidate))));
    }
    else
    {
        emit localCandidateChanged(&m_candidate);

        emit statusMessage(tr("Press Enter to add %1 to candidate list, or Control+Enter to commit to it immediately.").arg(QuackleIO::Util::moveToDetailedString(m_candidate)));
    }
}

void GraphicalBoardFrame::setLocalCandidate(const Quackle::Move *candidate)
{
    m_candidate = *candidate;
    resetArrow();
    prepare();
}

void GraphicalBoardFrame::backspaceHandler()
{
    unsigned int hoppedTiles = 0;
    QSize currentTile(m_arrowRoot);
    while (true)
    {
        const QSize previousTile = currentTile - arrowVector();

        bool stopHere;

        if (!isOnBoard(previousTile))
            stopHere = true;
        else
        {
            Quackle::Board::TileInformation previousTileInformation(m_board.tileInformation(previousTile.height(), previousTile.width()));

            stopHere = previousTileInformation.tileType != Quackle::Board::LetterTile;
        }

        ++hoppedTiles;

        if (stopHere)
        {
            m_arrowRoot = previousTile;
            break;
        }

        currentTile = previousTile;
    }

    Quackle::LetterString tiles(m_candidate.tiles());
    
    if (hoppedTiles > tiles.length())
        tiles = Quackle::LetterString();
    else
        tiles = Quackle::String::left(m_candidate.tiles(), m_candidate.tiles().length() - hoppedTiles);
    
    if (tiles.empty())
    {
        Quackle::Move originalMove;
        Quackle::LetterString hoppedLetters;

        currentTile = m_arrowRoot;
        while (true)
        {
            const QSize previousTile = currentTile - arrowVector();
    
            bool stopHere;

            if (!isOnBoard(previousTile))
                stopHere = true;
            else
            {
                Quackle::Board::TileInformation previousTileInformation(m_board.tileInformation(previousTile.height(), previousTile.width()));

                stopHere = previousTileInformation.tileType != Quackle::Board::LetterTile;
            }

            if (stopHere)
            {
                const bool horizontal = m_arrowDirection == ArrowRight;
                m_candidate = Quackle::Move::createPlaceMove(currentTile.height(), currentTile.width(), horizontal, hoppedLetters);
                break;
            }

            hoppedLetters += QUACKLE_PLAYED_THRU_MARK;
            currentTile = previousTile;
        }
    }
    else
    {
        m_candidate.setTiles(tiles);
    }

    ensureCandidatePlacedProperly();
    prettifyAndSetLocalCandidate(m_candidate);
}

void GraphicalBoardFrame::deleteHandler()
{
    const Quackle::Move move = Quackle::Move::createNonmove();
    setLocalCandidate(&move);
}

void GraphicalBoardFrame::submitHandler()
{
    QTimer::singleShot(0, this, [this] {setGlobalCandidate(nullptr);} );
}

void GraphicalBoardFrame::commitHandler()
{
    QTimer::singleShot(0, this, SLOT(setAndCommitGlobalCandidate()));
}

void GraphicalBoardFrame::setGlobalCandidate(bool *carryOn)
{
    if (m_candidate.action == Quackle::Move::Place && m_candidate.wordTilesWithNoPlayThru().empty())
    {
        emit statusMessage(tr("Click again to switch arrow orientation, or type a word."));
        return;
    }

    if (m_candidate.wordTilesWithNoPlayThru().length() == 1) 
    {
        Quackle::Move flippedMove = flip(m_candidate);
        emit setCandidateMove(&flippedMove, carryOn);
    }
    else
    {
        emit setCandidateMove(&m_candidate, carryOn);
    }
}

void GraphicalBoardFrame::setAndCommitGlobalCandidate()
{
    bool carryOn = false;
    setGlobalCandidate(&carryOn);
    if (carryOn)
        emit commit();
}

void GraphicalBoardFrame::appendHandler(const QString &text, bool shiftPressed)
{
    if (!isOnBoard(m_arrowRoot))
        return;

    if (!hasCandidate())
        return;

    Quackle::LetterString appendedLetterString(QuackleIO::Util::encode(text));

    if (appendedLetterString.length() == 0 || Quackle::String::front(appendedLetterString) < QUACKLE_FIRST_LETTER)
        return;
    
    if (shiftPressed)
        appendedLetterString = Quackle::String::setBlankness(appendedLetterString);
    else
        appendedLetterString = Quackle::String::clearBlankness(appendedLetterString);

    Quackle::Move newCandidate(m_candidate);
    Quackle::LetterString newTiles(m_candidate.tiles() + appendedLetterString);
    if (!m_ignoreRack && !m_rack.contains(Quackle::String::usedTiles(newTiles)))
    {
        Quackle::LetterString blankedNewTiles(m_candidate.tiles() + Quackle::String::setBlankness(appendedLetterString));

        if (m_rack.contains(Quackle::String::usedTiles(blankedNewTiles)))
        {
            newTiles = blankedNewTiles;
        }
    }

    newCandidate.setTiles(newTiles);

    Quackle::LetterString hoppedTiles;

    QSize currentTile(m_arrowRoot);
    while (true)
    {
        const QSize nextTile = currentTile + arrowVector();

        bool stopHere = false;
        bool resetArrowAfter = false;

        if (!isOnBoard(nextTile))
        {
            stopHere = true;
        }
        else
        {
            Quackle::Board::TileInformation nextTileInformation(m_board.tileInformation(nextTile.height(), nextTile.width()));

            if (nextTileInformation.tileType != Quackle::Board::LetterTile)
                stopHere = true;
        }

        if (stopHere)
        {
            newCandidate.setTiles(newCandidate.tiles() + hoppedTiles);

            if (resetArrowAfter)
                resetArrow();
            else
                m_arrowRoot = nextTile;

            break;
        }

        hoppedTiles += QUACKLE_PLAYED_THRU_MARK;
        currentTile = nextTile;
    }

    prettifyAndSetLocalCandidate(newCandidate);
}

void GraphicalBoardFrame::tileClicked(const QSize &tileLocation, const QMouseEvent * /* event */)
{
    Quackle::Board::TileInformation info(m_board.tileInformation(tileLocation.height(), tileLocation.width()));
    if (info.tileType == Quackle::Board::LetterTile)
        return;

    if (m_arrowRoot == tileLocation)
    {
        ++m_arrowDirection;
        if (m_arrowDirection == ArrowWorm)
            m_arrowDirection = s_firstArrowDirection;
    }
    else
        m_arrowDirection = s_firstArrowDirection;

    m_arrowRoot = tileLocation;

    Quackle::Move originalMove;
    Quackle::LetterString hoppedTiles;

    QSize currentTile(tileLocation);
    while (true)
    {
        const QSize previousTile = currentTile - arrowVector();

        bool stopHere;

        if (!isOnBoard(previousTile))
            stopHere = true;
        else
        {
            Quackle::Board::TileInformation previousTileInformation(m_board.tileInformation(previousTile.height(), previousTile.width()));

            stopHere = previousTileInformation.tileType != Quackle::Board::LetterTile;
        }

        if (stopHere)
        {
            const bool horizontal = m_arrowDirection == ArrowRight;
            originalMove = Quackle::Move::createPlaceMove(currentTile.height(), currentTile.width(), horizontal, hoppedTiles);
            break;
        }

        hoppedTiles += QUACKLE_PLAYED_THRU_MARK;
        currentTile = previousTile;
    }

    prettifyAndSetLocalCandidate(originalMove);

    // TODO work some cleverness so we can do this!
    //emit setCandidateMove(Quackle::Move::createNonmove());

    for (QSize currentTile(0, 0); currentTile.height() < m_boardSize.height(); currentTile.setHeight(currentTile.height() + 1))
        for (currentTile.setWidth(0); currentTile.width() < m_boardSize.width(); currentTile.setWidth(currentTile.width() + 1))
            if (currentTile != tileLocation)
            {
                TileWidget *tile = tileAt(currentTile);
                if (!tile)
                    continue;

                tile->setArrowDirection(NoArrow);
            }

    prepare();
}

void GraphicalBoardFrame::resetArrow()
{
    m_arrowRoot = QSize(-1, -1);
    m_arrowDirection = NoArrow;
}

bool GraphicalBoardFrame::hasArrow() const
{
    return m_arrowRoot.isValid();
}

QSize GraphicalBoardFrame::arrowVector() const
{
    const bool horizontal = (m_arrowDirection == ArrowRight);
    return horizontal? QSize(1, 0) : QSize(0, 1);
}

bool GraphicalBoardFrame::hasCandidate() const
{
    return m_candidate.isAMove();
}

void GraphicalBoardFrame::ensureCandidatePlacedProperly()
{
    if (m_candidate.tiles().empty())
    {
        m_candidate.startrow = m_arrowRoot.height();
        m_candidate.startcol = m_arrowRoot.width();
    }
}

bool GraphicalBoardFrame::isOnBoard(const QSize &location) const
{
    return (location.width() >= 0 && location.width() < m_boardSize.width() && location.height() >= 0 && location.height() < m_boardSize.height());
}

////////////////

unsigned int qHash(const QColor &color)
{
    return color.rgb();
}

PixmapCacher *PixmapCacher::m_self = 0;
PixmapCacher *PixmapCacher::self()
{
    if (m_self == 0)
        m_self = new PixmapCacher();

    return m_self;
}

void PixmapCacher::cleanUp()
{
    delete m_self;
    m_self = 0;
}

PixmapCacher::PixmapCacher()
{
    readTheme("");
}

void PixmapCacher::readTheme(const QString& themeFile)
{
    QSettings settings(themeFile, QSettings::IniFormat);

    arrowColor = QColor(settings.value("arrow", "black").toString());
    letterColor = QColor(settings.value("letter", "#6e6e6e").toString());
    britishLetterColor = QColor(settings.value("britishLetter", "#703d3d").toString());

    // tiles on rack will be of different sizes and thus are slightly
    // altered to fool the pixmap cacher
    rackColor = letterColor.lighter(101);

    DLSColor = QColor(settings.value("DLS", "cornflowerblue").toString());
    TLSColor = QColor(settings.value("TLS", "slateblue").toString());
    DWSColor = QColor(settings.value("DWS", "palevioletred").toString());
    TWSColor = QColor(settings.value("TWS", "firebrick").toString());
    QLSColor = QColor(settings.value("QLS", "blueviolet").toString());
    QWSColor = QColor(settings.value("QWS", "goldenrod").toString());
    bonusTextColor = QColor(settings.value("bonusLabel", "gainsboro").toString());

    nothingColor = QColor(settings.value("nothing", "gainsboro").toString());

    cementedLetterTextColor = QColor(settings.value("cementedLetterText", "#f8f8ff").toString());
    cementedBritishLetterTextColor = QColor(settings.value("cementedBritishLetterText", "#FFC0C0").toString());
    uncementedLetterTextColor = QColor(settings.value("uncementedLetterText", "khaki").toString());

    markColor = QColor(settings.value("mark", "tan").toString());
    markTextColor = QColor(settings.value("markLabel", "tan").toString());
}


bool PixmapCacher::contains(const QColor &color) const
{
    return m_pixmaps.contains(color);
}

QPixmap PixmapCacher::get(const QColor &color) const
{
    return m_pixmaps.value(color);
}

void PixmapCacher::put(const QColor &color, const QPixmap &pixmap)
{
    m_pixmaps.insert(color, pixmap);
}

void PixmapCacher::invalidate()
{
    m_pixmaps.clear();
}

////////////////

TileWidget::TileWidget()
    : m_cemented(false), m_arrowDirection(GraphicalBoardFrame::NoArrow), m_alwaysShowVerboseLabels(false)
{
}

TileWidget::~TileWidget()
{
}

void TileWidget::setSideLength(int sideLength)
{
    setOurSize(sideLength, sideLength);

    if (!m_pixmap.isNull())
        prepare();
}

int TileWidget::sideLength() const
{
    return m_size.width();
}

QSize TileWidget::size() const
{
    return m_size;
}

void TileWidget::setInformation(const Quackle::Board::TileInformation &information)
{
    m_information = information;
}

Quackle::Board::TileInformation TileWidget::information() const
{
    return m_information;
}

void TileWidget::setOriginalInformation(const Quackle::Board::TileInformation &originalInformation)
{
    if (originalInformation.tileType == Quackle::Board::BonusSquareTile)
        m_backgroundColor = tileColor(originalInformation);
}

void TileWidget::setLocation(const QSize &location)
{
    m_location = location;
}

void TileWidget::setDevicePixelRatio(qreal ratio)
{
    m_devicePixelRatio = ratio;
}

void TileWidget::setCemented(bool cemented)
{
    m_cemented = cemented;
}

bool TileWidget::cemented() const
{
    return m_cemented;
}

void TileWidget::setArrowDirection(int arrowDirection)
{
    m_arrowDirection = arrowDirection;
}

GraphicalBoardFrame::ArrowDirection TileWidget::arrowDirection() const
{
    return (GraphicalBoardFrame::ArrowDirection)m_arrowDirection;
}

void TileWidget::prepare()
{
    m_pixmap = generateTilePixmap();
}

const QPixmap &TileWidget::tilePixmap()
{
    return m_pixmap;
}

QColor TileWidget::tileColor()
{
    return tileColor(m_information);
}

QColor TileWidget::tileColor(const Quackle::Board::TileInformation &information)
{
    QColor ret;

    PixmapCacher *cache = PixmapCacher::self();

    switch (information.tileType)
    {
    case Quackle::Board::LetterTile:
        if (information.isOnRack)
            ret = cache->rackColor;
        else if (information.isBritish && QuackerSettings::self()->britishColoring == TileBritishColoring)
            ret = cache->britishLetterColor;
        else
            ret = cache->letterColor;
        break;

    case Quackle::Board::BonusSquareTile:
        switch (information.bonusSquareType)
        {
        case Quackle::Board::LetterBonus:
            if (information.bonusMultiplier == 2)
                ret = cache->DLSColor;
            else if (information.bonusMultiplier == 3)
                ret = cache->TLSColor;
            else if (information.bonusMultiplier == 4)
                ret = cache->QLSColor;
            else
            {
                // TODO general case
            }
            break;

        case Quackle::Board::WordBonus:
            if (information.bonusMultiplier == 2)
                ret = cache->DWSColor;
            else if (information.bonusMultiplier == 3)
                ret = cache->TWSColor;
            else if (information.bonusMultiplier == 4)
                ret = cache->QWSColor;
            else
            {
                // TODO general case
            }
            break;

        case Quackle::Board::NoBonus:
            // urps, this won't happen
            ret = cache->nothingColor;
            break;
        }
        break;

    case Quackle::Board::NothingTile:
        ret = cache->nothingColor;
        break;
    }
    
    return ret;
}

QColor TileWidget::backgroundColor()
{
    return m_backgroundColor;
}

QColor TileWidget::letterTextColor()
{
    QColor ret;

    if (m_arrowDirection != GraphicalBoardFrame::NoArrow)
        ret = PixmapCacher::self()->arrowColor;
    else if (m_cemented)
    {
        if (m_information.isBritish && QuackerSettings::self()->britishColoring == TextBritishColoring)
            ret = PixmapCacher::self()->cementedBritishLetterTextColor;
        else
            ret = PixmapCacher::self()->cementedLetterTextColor;
    }
    else if (m_information.letter == QUACKLE_NULL_MARK && m_information.isStartLocation)
        ret = PixmapCacher::self()->arrowColor;
    else if (m_information.letter == QUACKLE_NULL_MARK)
        ret = PixmapCacher::self()->bonusTextColor;
    else
        ret = PixmapCacher::self()->uncementedLetterTextColor;

    return ret;
}

QString TileWidget::letterText()
{
    int glyphIndex;

    if (m_arrowDirection == GraphicalBoardFrame::ArrowRight)
        glyphIndex = 0;
    else if (m_arrowDirection == GraphicalBoardFrame::ArrowDown)
        glyphIndex = 1;
    else if (m_information.letter == QUACKLE_NULL_MARK)
    {
        if (m_information.isStartLocation)
            glyphIndex = 2;
        else
        {
            if (shouldShowVerboseLabels())
            {
                if (m_information.bonusSquareType == Quackle::Board::LetterBonus)
                    return QString("%1LS").arg(m_information.bonusMultiplier);
                else if (m_information.bonusSquareType == Quackle::Board::WordBonus)
                    return QString("%1WS").arg(m_information.bonusMultiplier);
            }

            return QString();
        }
    }
    else
        return QuackleIO::Util::sanitizeUserVisibleLetterString(QuackleIO::Util::letterToQString(m_information.letter));
    
#ifdef FORCE_SECONDARY_ARROW_GLYPHS
    QChar macSecondaryGlyphs[] =  {QChar(0x2192), QChar(0x2193), QChar(0x2606)}; // single arrows, white star
    return QString(macSecondaryGlyphs[glyphIndex]);
#else
    QChar preferredGlyphs[] =  {QChar(0x21d2), QChar(0x21d3), QChar(0x2606)}; // double arrows, white star
    QChar secondaryGlyphs[] =  {QChar(0x2192), QChar(0x2193), QChar(   '*')}; // single arrows, ASCII star
    QChar asciiGlyphs[] =      {QChar(   '>'), QChar(   'v'), QChar(   '*')}; // 7-bit
    QFontMetrics metrics(letterFont());

    if (metrics.inFont(preferredGlyphs[0]) &&
            metrics.inFont(preferredGlyphs[1]) &&
            metrics.inFont(preferredGlyphs[2]))
        return QString(preferredGlyphs[glyphIndex]);
    else if (metrics.inFont(secondaryGlyphs[0]) &&
            metrics.inFont(secondaryGlyphs[1]) &&
            metrics.inFont(secondaryGlyphs[2]))
        return QString(secondaryGlyphs[glyphIndex]);
    else
        return QString(asciiGlyphs[glyphIndex]);
#endif
}

QFont TileWidget::letterFont()
{
    // Sorry for the hardcoded constants :( :(
    if (m_information.isStartLocation && m_information.tileType != Quackle::Board::LetterTile)
        return scaledFont(0.7);
    else if (m_arrowDirection == GraphicalBoardFrame::NoArrow && m_information.tileType == Quackle::Board::BonusSquareTile)
        return scaledFont(.35);
    else
    {
        double initialScale = 0.7;
        if (m_information.tileType == Quackle::Board::LetterTile)
        {
            QString text = letterText();
            if (text.length() > 1)
            {
                initialScale = 1.0 / text.length();
            }

        }

        double scale = m_information.isBlank? initialScale * 5 / 7 : initialScale;

        QFont ret = scaledFont(scale);
        ret.setBold(true);
        return ret;
    }
}

QFont TileWidget::actualLetterFont()
{
    return scaledFont(1);
}

QString TileWidget::miniText()
{
    if (m_information.letter != QUACKLE_NULL_MARK && QuackerSettings::self()->scoreLabels)
    {   
        if (m_information.isBlank)
            return QString();
        else
            return QString::number(QUACKLE_ALPHABET_PARAMETERS->score(m_information.letter));
    }
    else
        return QString();
}

QColor TileWidget::miniTextColor()
{
    return tileColor().lighter(170);
}

QFont TileWidget::miniFont()
{
    return scaledFont(miniText().length() > 1? .275 : .325);
}

bool TileWidget::shouldShowVerboseLabels() const
{
    return m_alwaysShowVerboseLabels || QuackerSettings::self()->verboseLabels;
}

QFont TileWidget::scaledFont(double multiplier)
{
    const int smallerSideLength = qMin(size().width(), size().height());

    if (smallerSideLength == 0)
        return PixmapCacher::self()->tileFont;

    QFont ret(PixmapCacher::self()->tileFont);
    ret.setPixelSize((int)(smallerSideLength * multiplier));
    return ret;
}

void TileWidget::setOurSize(const QSize &size)
{
    m_size = size;
}

void TileWidget::setOurSize(int width, int height)
{
    setOurSize(QSize(width, height));
}

QPixmap TileWidget::generateTilePixmap()
{
    const QSize currentSize(size());

    if (currentSize.isEmpty())
        return QPixmap();

    const QRect borderRect(0, 0, currentSize.width(), currentSize.height());
    const int maxSideLength = qMax(currentSize.width(), currentSize.height());
    const int borderWidth = 1;

    const QPointF midpoint((double)(currentSize.width() + borderWidth) / 2, (double)(currentSize.height() + borderWidth) / 2);
    const QColor color(tileColor());

    QPixmap ret(currentSize * m_devicePixelRatio);
    ret.setDevicePixelRatio(m_devicePixelRatio);

    if (PixmapCacher::self()->contains(color))
        ret = PixmapCacher::self()->get(color);
    else
    {
        //UVcout << "cache miss for color " << color.rgb() << endl;
        double radius = maxSideLength / 2 * 1.41 + maxSideLength / 10 + 2;

        // could be used for cool effect -- the color of the bonus square we're obscuring
        //const QColor outerColor(backgroundColor());

        QRadialGradient gradient(QPointF(radius, radius), radius * 3, QPointF(radius / 3, radius / 3));
        gradient.setColorAt(0, color.lighter(GraphicalBoardFrame::s_highlightFactor));
        gradient.setColorAt(.95, color.darker(GraphicalBoardFrame::s_highlightFactor));

        QPainter painter(&ret);
        painter.setBrush(gradient);
    
        painter.drawEllipse((int)(midpoint.x() - radius), (int)(midpoint.y() - radius), (int)(radius * 2), (int)(radius * 2));

        QPalette customPalette;
        customPalette.setColor(QPalette::Light, color.lighter(GraphicalBoardFrame::s_highlightFactor));
        customPalette.setColor(QPalette::Dark, color);
        customPalette.setColor(QPalette::Mid, color);

        qDrawShadePanel(&painter, borderRect.x(), borderRect.y(), borderRect.width(), borderRect.height(), customPalette, false, borderWidth);

        PixmapCacher::self()->put(color, ret);
    }

    const QString nanism = miniText();
    const bool hasNanism = !nanism.isEmpty();

    const QString text = letterText();
    if (!text.isEmpty())
    {
        QPainter painter(&ret);
        painter.setFont(letterFont());
        QPen pen(letterTextColor());
        painter.setPen(pen);
        painter.setBrush(Qt::NoBrush);

        const QRectF textSize(painter.boundingRect(borderRect, text));
        const QPointF startPoint(midpoint - textSize.bottomRight() / 2);
        const QPointF roundedStartPoint(floor(startPoint.x()), floor(startPoint.y()));

        QRectF textRect(textSize);
        textRect.moveTo(roundedStartPoint);

        painter.drawText(textRect, Qt::TextDontClip, text);

        if (m_information.isBlank)
        {
            painter.setBrush(Qt::NoBrush);
            pen.setWidth(1);
            painter.setPen(pen);

            const int border = currentSize.width() / 5;
            painter.drawRect(QRect(border, border, currentSize.width() - 2 * border, currentSize.height() - 2 * border));
        }
    }

    if (hasNanism)
    {
        QPainter painter(&ret);
        painter.setFont(miniFont());
        QPen pen(miniTextColor());
        painter.setPen(pen);
        painter.setBrush(Qt::NoBrush);

        const QRectF textSize(painter.boundingRect(borderRect, nanism));
        const QPointF startPoint((midpoint * (nanism.length() > 1? 1.65 : 1.68)) - textSize.bottomRight() / 2);
        const QPointF roundedStartPoint(floor(startPoint.x()), floor(startPoint.y()));

        QRectF textRect(textSize);
        textRect.moveTo(roundedStartPoint);

        painter.drawText(textRect, Qt::TextDontClip, nanism);
    }

    return ret;
}

bool operator<(const QSize &firstSize, const QSize &secondSize)
{
    return (firstSize.height() + (QUACKLE_MAXIMUM_BOARD_SIZE + 1) * firstSize.width()) < (secondSize.height() + (QUACKLE_MAXIMUM_BOARD_SIZE + 1) * secondSize.width());
}

//////////////////

MarkWidget::MarkWidget()
    : m_horizontal(true), m_capstone(false)
{
    m_information.tileType = Quackle::Board::LetterTile;
}

MarkWidget::~MarkWidget()
{
}

void MarkWidget::setRow(int row)
{
    m_horizontal = false;
    m_letterText = QString::number(row);
}

void MarkWidget::setCol(int col)
{
    m_horizontal = true;
    m_letterText = QChar('A' + col - 1);
}

void MarkWidget::setCapstone()
{
    m_capstone = true;
    m_letterText = QString();
}

void MarkWidget::setSideLength(int sideLength)
{
    const int otherLength = (int)(sideLength * GraphicalBoardFrame::s_markOtherLengthMultiplier);

    if (m_capstone)
        setOurSize(otherLength, otherLength);
    else if (m_horizontal)
        setOurSize(sideLength, otherLength);
    else
        setOurSize(otherLength, sideLength);

    prepare();
}

QColor MarkWidget::tileColor()
{
    // we slightly alter colors to fool the pixmap cacher!
    if (m_capstone)
        return PixmapCacher::self()->markColor.lighter(101);
    else if (m_horizontal)
        return PixmapCacher::self()->markColor;
    else
        return PixmapCacher::self()->markColor.darker(101);
}

QColor MarkWidget::letterTextColor()
{
    return PixmapCacher::self()->markTextColor;
}

QFont MarkWidget::letterFont()
{
    return scaledFont(0.7);
}

QString MarkWidget::letterText()
{
    return m_letterText;
}
