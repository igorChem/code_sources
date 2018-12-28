/**********************************************************************
  ConstraintsDialog - Dialog for force field constraint settings

  Copyright (C) 2007 by Tim Vandermeersch

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

#include "constraintsdialog.h"

#include <avogadro/molecule.h>

#include <QPushButton>
#include <QButtonGroup>
#include <QDebug>

#include <QFileDialog>
#include <QFile>

#include <QMessageBox>

namespace Avogadro {

  ConstraintsDialog::ConstraintsDialog( QWidget *parent, Qt::WindowFlags f ) : QDialog( parent, f )
  {
    ui.setupUi(this);

    connect( ui.ConstraintsOK, SIGNAL( clicked() ), this, SLOT( acceptConstraints() ));
    connect( ui.ConstraintsAdd, SIGNAL( clicked() ), this, SLOT( addConstraint() ));
    connect( ui.ConstraintsDelete, SIGNAL( clicked() ), this, SLOT( deleteConstraint() ));
    connect( ui.ConstraintsDeleteAll, SIGNAL( clicked() ), this, SLOT( deleteAllConstraints() ));
    connect( ui.comboType, SIGNAL( currentIndexChanged(int) ), this, SLOT( comboTypeChanged(int) ));
    
    m_forceField = OpenBabel::OBForceField::FindForceField( "MMFF94" );
    
    ui.editValue->setMinimum(0.0);
    ui.editValue->setMaximum(0.0);

    // make sure the table stretches across the dialog as it resizes
    QHeaderView *horizontal = ui.ConstraintsTableView->horizontalHeader();
    horizontal->setResizeMode(QHeaderView::Stretch);
    QHeaderView *vertical = ui.ConstraintsTableView->verticalHeader();
    vertical->setResizeMode(QHeaderView::Stretch);

  }

  void ConstraintsDialog::showEvent(QShowEvent *event)
  {
    Q_UNUSED(event);

    switch (ui.comboType->currentIndex()) {
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
      if (m_molecule->numAtoms() >= 1) {
        ui.editA->setMinimum(1);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(m_molecule->numAtoms());
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      } else {
        ui.editA->setMinimum(0);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(0);
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      }
      break;
    case 5:
      if (m_molecule->numAtoms() >= 2) {
        ui.editA->setMinimum(1);
        ui.editB->setMinimum(1);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(m_molecule->numAtoms() - 1);
        ui.editB->setMaximum(m_molecule->numAtoms());
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editB->setValue(2);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(5.0);
        ui.editValue->setSingleStep(0.05);
        ui.editValue->setValue(1.5);
      } else {
        ui.editA->setMinimum(0);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(0);
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      }
      break;
    case 6:
      if (m_molecule->numAtoms() >= 3) {
        ui.editA->setMinimum(1);
        ui.editB->setMinimum(1);
        ui.editC->setMinimum(1);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(m_molecule->numAtoms() - 2);
        ui.editB->setMaximum(m_molecule->numAtoms() - 1);
        ui.editC->setMaximum(m_molecule->numAtoms());
        ui.editD->setMaximum(0);
        ui.editB->setValue(2);
        ui.editC->setValue(3);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(180.0);
        ui.editValue->setSingleStep(1.0);
        ui.editValue->setValue(109.0);	
      } else {
        ui.editA->setMinimum(0);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(0);
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      }
      break;
    case 7:
      if (m_molecule->numAtoms() >= 4) {
        ui.editA->setMinimum(1);
        ui.editB->setMinimum(1);
        ui.editC->setMinimum(1);
        ui.editD->setMinimum(1);
        ui.editA->setMaximum(m_molecule->numAtoms() - 3);
        ui.editB->setMaximum(m_molecule->numAtoms() - 2);
        ui.editC->setMaximum(m_molecule->numAtoms() - 1);
        ui.editD->setMaximum(m_molecule->numAtoms());
        ui.editB->setValue(2);
        ui.editC->setValue(3);
        ui.editD->setValue(4);
        ui.editValue->setMinimum(-180.0);
        ui.editValue->setMaximum(180.0);
        ui.editValue->setSingleStep(10.0);
        ui.editValue->setValue(0.0);
      } else {
        ui.editA->setMinimum(0);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(0);
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      }
      break;
    }
 
  }

  ConstraintsDialog::~ConstraintsDialog()
  {
  }

  void ConstraintsDialog::setModel(ConstraintsModel *model)
  {
    m_constraints = model;
    ui.ConstraintsTableView->setModel(m_constraints);
  }
  
  void ConstraintsDialog::setMolecule(Molecule *molecule)
  {
    m_molecule = molecule;
    connect(m_molecule, SIGNAL( primitiveRemoved(Primitive *) ), m_constraints, SLOT( primitiveRemoved(Primitive *) ));
  }
  
  void ConstraintsDialog::comboTypeChanged(int index)
  {
    switch (index) {
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
      if (m_molecule->numAtoms() >= 1) {
        ui.editA->setMinimum(1);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(m_molecule->numAtoms());
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      } else {
        ui.editA->setMinimum(0);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(0);
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      }
      break;
    case 5:
      if (m_molecule->numAtoms() >= 2) {
        ui.editA->setMinimum(1);
        ui.editB->setMinimum(1);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(m_molecule->numAtoms() - 1);
        ui.editB->setMaximum(m_molecule->numAtoms());
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editB->setValue(2);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(5.0);
        ui.editValue->setSingleStep(0.05);
        ui.editValue->setValue(1.5);
      } else {
        ui.editA->setMinimum(0);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(0);
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      }
      break;
    case 6:
      if (m_molecule->numAtoms() >= 3) {
        ui.editA->setMinimum(1);
        ui.editB->setMinimum(1);
        ui.editC->setMinimum(1);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(m_molecule->numAtoms() - 2);
        ui.editB->setMaximum(m_molecule->numAtoms() - 1);
        ui.editC->setMaximum(m_molecule->numAtoms());
        ui.editD->setMaximum(0);
        ui.editB->setValue(2);
        ui.editC->setValue(3);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(180.0);
        ui.editValue->setSingleStep(10.0);
        ui.editValue->setValue(109.0);
      } else {
        ui.editA->setMinimum(0);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(0);
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      }
      break;
    case 7:
      if (m_molecule->numAtoms() >= 4) {
        ui.editA->setMinimum(1);
        ui.editB->setMinimum(1);
        ui.editC->setMinimum(1);
        ui.editD->setMinimum(1);
        ui.editA->setMaximum(m_molecule->numAtoms() - 3);
        ui.editB->setMaximum(m_molecule->numAtoms() - 2);
        ui.editC->setMaximum(m_molecule->numAtoms() - 1);
        ui.editD->setMaximum(m_molecule->numAtoms());
        ui.editB->setValue(2);
        ui.editC->setValue(3);
        ui.editD->setValue(4);
        ui.editValue->setMinimum(-180.0);
        ui.editValue->setMaximum(180.0);
        ui.editValue->setSingleStep(10.0);
        ui.editValue->setValue(0.0);
      } else {
        ui.editA->setMinimum(0);
        ui.editB->setMinimum(0);
        ui.editC->setMinimum(0);
        ui.editD->setMinimum(0);
        ui.editA->setMaximum(0);
        ui.editB->setMaximum(0);
        ui.editC->setMaximum(0);
        ui.editD->setMaximum(0);
        ui.editValue->setMinimum(0.0);
        ui.editValue->setMaximum(0.0);
      }
      break;
    }
  
  }

  void ConstraintsDialog::acceptConstraints()
  {
    hide();
  }

  void ConstraintsDialog::deleteConstraint()
  {
    m_constraints->deleteConstraint(ui.ConstraintsTableView->currentIndex().row());
    m_forceField->SetConstraints(m_constraints->constraints());
  }

  void ConstraintsDialog::deleteAllConstraints()
  {
    m_constraints->clear();
    m_forceField->SetConstraints(m_constraints->constraints());
    this->update();
  }
  
  void ConstraintsDialog::addConstraint()
  {
    if (!m_molecule->numAtoms()) {
      QMessageBox::warning(static_cast<QWidget*>(parent()), tr("Add constraint"), 
                           tr("Your molecule must contain at least one atom to add a constraint"));
      return;
    }
    
    switch (ui.comboType->currentIndex()) {
    case 0: // Ignore
      m_constraints->addIgnore(ui.editA->value());
      break;
    case 1: // Atom 
      m_constraints->addAtomConstraint(ui.editA->value());
      break;
    case 2: // Atom X 
      m_constraints->addAtomXConstraint(ui.editA->value());
      break;
    case 3: // Atom Y 
      m_constraints->addAtomYConstraint(ui.editA->value());
      break;
    case 4: // Atom Z 
      m_constraints->addAtomZConstraint(ui.editA->value());
      break;
    case 5: // Distance
      if (m_molecule->numAtoms() < 2) {
        QMessageBox::warning(static_cast<QWidget*>(parent()), tr("Add constraint"), 
                             tr("Your molecule must contain at least two atoms to add a bond constraint"));
        break;
      }
      m_constraints->addDistanceConstraint(ui.editA->value(),
                                           ui.editB->value(), ui.editValue->value());
      break;
    case 6: // Angle
      if (m_molecule->numAtoms() < 3) {
        QMessageBox::warning(static_cast<QWidget*>(parent()), tr("Add constraint"), 
                             tr("Your molecule must contain at least three atoms to add an angle constraint"));
        break;
      }
	
      m_constraints->addAngleConstraint(ui.editA->value(),
                                        ui.editB->value(), ui.editC->value(), ui.editValue->value());
      break;
    case 7: // Torsion
      if (m_molecule->numAtoms() < 4) {
        QMessageBox::warning(static_cast<QWidget*>(parent()), tr("Add constraint"), 
                             tr("Your molecule must contain at least four atoms to add a torsion constraint"));
        break;
      }
	
      m_constraints->addTorsionConstraint(ui.editA->value(),
                                          ui.editB->value(), ui.editC->value(), ui.editD->value(),
                                          ui.editValue->value());
      break;
    }
  
    m_forceField->SetConstraints(m_constraints->constraints());
  }


}

