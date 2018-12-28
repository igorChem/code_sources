/**********************************************************************
  ZMatrixTool - ZMatrix Manipulation Tool for Avogadro

  Copyright (C) 2009 by Marcus D. Hanwell

  This file is part of the Avogadro molecular editor project.
  For more information, see <http://avogadro.cc/>

  Avogadro is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Avogadro is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
  02110-1301, USA.
 **********************************************************************/

#ifndef ZMATRIXDIALOG_H
#define ZMATRIXDIALOG_H

#include <avogadro/global.h>

#include <QDialog>
#include <QPointer>

#include "ui_zmatrixdialog.h"

namespace Avogadro {

  class Molecule;
  class GLWidget;
  class ZMatrixModel;

  class ZMatrixDialog : public QDialog
  {
    Q_OBJECT
  public:
    explicit ZMatrixDialog(QWidget *parent = 0, Qt::WindowFlags f = 0);
    ~ZMatrixDialog();

  public slots:
    void setMolecule(Molecule *molecule);
    void setGLWidget(GLWidget *widget);

  private:
    Ui::ZMatrixDialog ui;
    ZMatrixModel *m_zMatrixModel;
    QPointer<Molecule> m_molecule;
    QPointer<GLWidget> m_glwidget;

  private slots:
    void addAtom();
    void removeAtom();
    void importSelectedAtoms();
  };

} // End namespace Avogadro

#endif // ZMATRIXDIALOG_H
