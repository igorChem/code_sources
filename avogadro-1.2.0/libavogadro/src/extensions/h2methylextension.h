/**********************************************************************
  H2Methyl - Hydrogen to Methyl plugin for Avogadro

  Copyright (C) 2006 by Donald Ephraim Curtis
  Copyright (C) 2006-2007 by Geoffrey R. Hutchison

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

#ifndef H2METHYLEXTENSION_H
#define H2METHYLEXTENSION_H

#include <avogadro/extension.h>

#include <avogadro/primitivelist.h>

#include <QUndoCommand>

namespace Avogadro {

 class H2MethylExtension : public Extension
  {
    Q_OBJECT
      AVOGADRO_EXTENSION("H2Methyl", tr("H2Methyl"),
                         tr("Transform hydrogens to methyl groups"))

    public:
      //! Constructor
      H2MethylExtension(QObject *parent=0);
      //! Destructor
      virtual ~H2MethylExtension();

      //! Perform Action
      virtual QList<QAction *> actions() const;
      virtual QUndoCommand* performAction(QAction *action, GLWidget *widget);
      virtual QString menuPath(QAction *action) const;

      virtual void setMolecule(Molecule *molecule);
      //@}

    private:
      QList<QAction *> m_actions;

      Molecule *m_molecule;
  };

  class H2MethylCommand : public QUndoCommand
  {
    public:
      H2MethylCommand(Molecule *molecule, GLWidget *widget);
      ~H2MethylCommand();

      virtual void undo();
      virtual void redo();
      virtual bool mergeWith ( const QUndoCommand * command );
      virtual int id() const;

    private:
      Molecule *m_molecule;
      Molecule *m_moleculeCopy;
      PrimitiveList m_SelectedList;
  };

  class H2MethylExtensionFactory : public QObject, public PluginFactory
  {
      Q_OBJECT
      Q_INTERFACES(Avogadro::PluginFactory)
      AVOGADRO_EXTENSION_FACTORY(H2MethylExtension)
  };


} // end namespace Avogadro

#endif
