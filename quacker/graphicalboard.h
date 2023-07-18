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

#ifndef QUACKER_GRAPHICALBOARD_H
#define QUACKER_GRAPHICALBOARD_H

#include <QColor>
#include <QWidget>
#include <QHash>
#include <QMap>
#include <QPixmap>
#include <QSize>

#include <board.h>

#include "boarddisplay.h"

class QFrame;
class QGridLayout;

class GraphicalBoardFrame;
class MarkWidget;
class TileWidget;

namespace Quackle
{
    class Board;
    class Move;
}

unsigned int qHash(const QColor &color);

class PixmapCacher
{
public:
    static PixmapCacher *self();

    // deletes self and resets
    static void cleanUp();

    void readTheme(const QString& themeFile);

    QColor arrowColor;
    QColor letterColor;
    QColor britishLetterColor;
    QColor DLSColor;
    QColor TLSColor;
    QColor DWSColor;
    QColor TWSColor;
    QColor QLSColor;
    QColor QWSColor;
    QColor bonusTextColor;
    
    QColor nothingColor;
    QColor rackColor;

    QColor cementedLetterTextColor;
    QColor cementedBritishLetterTextColor;
    QColor uncementedLetterTextColor;

    QColor markColor;
    QColor markTextColor;

    QFont tileFont;

    bool contains(const QColor &color) const;
    QPixmap get(const QColor &color) const;
    void put(const QColor &color, const QPixmap &pixmap);

    void invalidate();

protected:
    PixmapCacher();
    PixmapCacher(const PixmapCacher&);
    PixmapCacher& operator=(const PixmapCacher&);
    
private:
    static PixmapCacher *m_self;
    QHash<QColor, QPixmap> m_pixmaps;
};

class GraphicalBoard : public BoardWithQuickEntry
{
Q_OBJECT

public:
    GraphicalBoard(QWidget *parent = 0);
    ~GraphicalBoard();

    GraphicalBoardFrame *boardFrame() { return m_boardFrame; }

protected slots:
    virtual void expandToFullWidth();
    virtual void resizeEvent(QResizeEvent *event);

private:
    GraphicalBoardFrame *m_boardFrame;
    QWidget *m_boardWrapper;
};

class GraphicalBoardFrame : public View
{
Q_OBJECT

public:
    GraphicalBoardFrame(QWidget *parent = 0);
    ~GraphicalBoardFrame();

    static const double s_markOtherLengthMultiplier;
    static const int s_highlightFactor = 125;

    // ArrowRight is direction after initial click;
    // when we increment to ArrowWorm we restart with it
    enum ArrowDirection { NoArrow = 0, ArrowRight = 1, ArrowDown = 2, /* ... */ ArrowWorm = 3 };

    static const int s_firstArrowDirection = ArrowRight;

    static void staticDrawPosition(const Quackle::GamePosition &position, const QSize &size, QPixmap *pixmap);

public slots:
    virtual void positionChanged(const Quackle::GamePosition *position);
    virtual void expandToSize(const QSize &maxSize);

protected slots:
    void backspaceHandler();
    void deleteHandler();
    void submitHandler();
    void commitHandler();
    void appendHandler(const QString &text, bool shiftPressed);

    void setGlobalCandidate(bool *carryOn); // returns false if the user canceled a badly formed move
    void setAndCommitGlobalCandidate();

    virtual void tileClicked(const QSize &tileLocation, const QMouseEvent * /* event */);
    virtual void prepare();

    void setLocalCandidate(const Quackle::Move *candidate);

protected:
    Quackle::Board m_board;
    Quackle::Rack m_rack;
    bool m_ignoreRack;

    virtual void keyPressEvent(QKeyEvent *e);
    virtual void paintEvent(QPaintEvent *event);
    virtual void mousePressEvent(QMouseEvent *event);
    
    virtual bool wantMousePressEvent(const QMouseEvent *event) const;

    void generateBoardPixmap(QPixmap *pixmap);

    // these three are misnamed - they just set up the fields of the
    // tilewidgets
    void drawBoard(const Quackle::Board &board);
    void drawMove(const Quackle::Move &move);
    void drawArrow(const QSize &location, int arrowDirection);

    void deleteWidgets();
    void recreateWidgets();
    void resizeWidgets(int sideLength);
    void flushPixmapsAndRedraw();

    void addTile(const QSize &loc, TileWidget *tile);
    void removeTile(const QSize &loc);
    TileWidget *tileAt(const QSize &loc);

    // returns invalid size if there no tile at point
    // or the tile at relative-to-widget coordinates pos
    QSize locationForPosition(const QPoint &pos);

    QPoint coordinatesOfTile(const QSize &loc);

    void addMark(const QSize &loc, MarkWidget *tile);
    void removeMark(const QSize &loc);
    TileWidget *markAt(const QSize &loc);
    QPoint coordinatesOfMark(const QSize &loc);

    Quackle::Move flip(const Quackle::Move &flippee);
    void prettifyAndSetLocalCandidate(const Quackle::Move &candidate);

    bool m_alwaysShowVerboseLabels;

signals:
    void localCandidateChanged(const Quackle::Move *candidate);
    void tileFontChanged(const QFont &font);

private:
    QMap<QSize, TileWidget *> m_tileWidgets;
    QSize m_boardSize;
    QSize m_sizeForBoard;

    QPixmap m_pixmap;

    // when empty, user has set no arrow
    QSize m_arrowRoot;
    Quackle::Move m_candidate;
    int m_arrowDirection;

    void resetArrow();
    bool hasArrow() const;
    QSize arrowVector() const;
    bool hasCandidate() const;
    void ensureCandidatePlacedProperly();

    bool isOnBoard(const QSize &location) const;

    QSize m_maxSize;

    // side length of one tile
    int m_sideLength;

    // shorter side length of a mark
    QPoint m_tilesOffset;
};

class TileWidget
{
public:
    TileWidget();
    virtual ~TileWidget();

    virtual void setSideLength(int sideLength);
    int sideLength() const;

    QSize size() const;

    virtual void setInformation(const Quackle::Board::TileInformation &information);
    Quackle::Board::TileInformation information() const;

    virtual void setOriginalInformation(const Quackle::Board::TileInformation &originalInformation);
    virtual void setLocation(const QSize &location);
    virtual void setDevicePixelRatio(qreal ratio);

    virtual void setCemented(bool cemented);
    bool cemented() const;

    virtual void setArrowDirection(int arrowDirection);
    GraphicalBoardFrame::ArrowDirection arrowDirection() const;

    // to be called after the set* functions to show
    // the correct things
    virtual void prepare();

    const QPixmap &tilePixmap();

    virtual QColor tileColor();
    virtual QColor tileColor(const Quackle::Board::TileInformation &information);
    virtual QColor backgroundColor();
    virtual QColor letterTextColor();
    virtual QString letterText();

    // either actual font or smaller one for bonus square message
    virtual QFont letterFont();

    // font for drawing actual letters
    virtual QFont actualLetterFont();
    
    virtual QString miniText();
    virtual QColor miniTextColor();
    virtual QFont miniFont();

    void setAlwaysShowVerboseLabels(bool alwaysShowVerboseLabels);

protected:
    QPixmap generateTilePixmap();
    QFont scaledFont(double multiplier);
    static const double s_defaultLetterScale;

    virtual void setOurSize(const QSize &size);
    virtual void setOurSize(int width, int height);

    Quackle::Board::TileInformation m_information;
    bool m_cemented;
    int m_arrowDirection;

    QColor m_backgroundColor;
    QSize m_location;

    QSize m_size;
    QPixmap m_pixmap;
    qreal m_devicePixelRatio = 1.0;

    bool shouldShowVerboseLabels() const;

    bool m_alwaysShowVerboseLabels;
};

inline void TileWidget::setAlwaysShowVerboseLabels(bool alwaysShowVerboseLabels)
{
    m_alwaysShowVerboseLabels = alwaysShowVerboseLabels;
}

class MarkWidget : public TileWidget
{
public:
    MarkWidget();
    virtual ~MarkWidget();

    virtual void setSideLength(int sideLength);
    virtual QColor tileColor();
    virtual QColor letterTextColor();
    virtual QFont letterFont();
    virtual QString letterText();

    void setRow(int row);
    void setCol(int col);
    void setCapstone();

protected:
    bool m_horizontal;
    bool m_capstone;
    QString m_letterText;
};

bool operator<(const QSize &firstSize, const QSize &secondSize);

#endif
