/**********************************************************************
  MoleculeDelegate - Project Tree Items for molecule (atoms, bonds, ...).

  Copyright (C) 2008 by Tim Vandermeersch

  This file is part of the Avogadro molecular editor project.
  For more information, see <http://avogadro.cc/>

  Some code is based on Open Babel
  For more information, see <http://openbabel.sourceforge.net/>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation version 2 of the License.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
 ***********************************************************************/

#ifndef MOLECULEDELEGATE_H
#define MOLECULEDELEGATE_H

#include <avogadro/global.h>
#include "../projecttreemodeldelegate.h"
#include "../projecttreemodel.h"

#include <QString>

namespace Avogadro {

  class Primitive;

  class MoleculeDelegate : public ProjectTreeModelDelegate
  {
    Q_OBJECT

    public:
      MoleculeDelegate(ProjectTreeModel *model);
      ~MoleculeDelegate();

      QString name() const { return QObject::tr("Molecule"); }
      
      void initStructure(GLWidget *, ProjectTreeItem *parent);
      
      void writeSettings(QSettings &settings) const;
      void readSettings(QSettings &settings);
  };

} // end namespace Avogadro

#endif
