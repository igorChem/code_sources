/**********************************************************************
  StickEngine - Engine for "stick" display

  Copyright (C) 2006-2008 Geoffrey R. Hutchison
  Copyright (C) 2008 Marcus D. Hanwell

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

#include "stickengine.h"

#include <avogadro/primitive.h>
#include <avogadro/atom.h>
#include <avogadro/bond.h>
#include <avogadro/molecule.h>
#include <avogadro/color.h>
#include <avogadro/glwidget.h>
#include <avogadro/painterdevice.h>
#include <avogadro/camera.h>

#include <openbabel/mol.h>

#include <QMessageBox>

using namespace Eigen;

// Conversion from integers to double
// Default has been 20.0
// i.e., smallest radius = 0.05 (1/20), default is 0.25 (5/20), max is 0.5 (10/20)
#define SCALING_FACTOR  20.0

namespace Avogadro {

  StickEngine::StickEngine(QObject *parent) : Engine(parent), m_settingsWidget(0),
        m_radius(0.25)
  {
  }

  StickEngine::~StickEngine()
  {
    // Delete the settings widget if it exists
    if(m_settingsWidget)
      m_settingsWidget->deleteLater();
  }

  Engine* StickEngine::clone() const
  {
    StickEngine* engine = new StickEngine(parent());

    engine->setAlias(alias());
    engine->setEnabled(isEnabled());
    engine->setRadius(m_radius * SCALING_FACTOR);
    return engine;
  }

  bool StickEngine::renderOpaque(PainterDevice *pd)
  {
//    glPushAttrib(GL_TRANSFORM_BIT);
    glDisable( GL_NORMALIZE );
    glEnable( GL_RESCALE_NORMAL );

    // Render the atoms
    foreach(Atom *a, atoms())
      renderOpaque(pd, a);

    // render bonds (sticks)
    glDisable( GL_RESCALE_NORMAL );
    glEnable( GL_NORMALIZE );
    foreach(Bond *b, bonds())
      renderOpaque(pd, b);

//    glPopAttrib();

    return true;
  }

  bool StickEngine::renderTransparent(PainterDevice *pd)
  {
    glDisable( GL_NORMALIZE );
    glEnable( GL_RESCALE_NORMAL );

    Color *map = colorMap(); // possible custom color map
    if (!map) map = pd->colorMap(); // fall back to global color map
    map->setToSelectionColor();
    pd->painter()->setColor(map);

    // Render the atoms
    foreach(Atom *a, atoms()) {
      if (pd->isSelected(a)) {
        pd->painter()->setName(a);
        pd->painter()->drawSphere(a->pos(), SEL_ATOM_EXTRA_RADIUS + radius(a));
      }
    }

    // render bonds (sticks)
    glDisable( GL_RESCALE_NORMAL );
    glEnable( GL_NORMALIZE );
    foreach(Bond *b, bonds()) {
      if (pd->isSelected(b)) {
        Atom* atom1 = pd->molecule()->atomById(b->beginAtomId());
        Atom* atom2 = pd->molecule()->atomById(b->endAtomId());
        Vector3d v1 (*atom1->pos());
        Vector3d v2 (*atom2->pos());
        Vector3d v3 (( v1 + v2 ) / 2);
        pd->painter()->setName(b);
        pd->painter()->drawCylinder(v1, v2, SEL_BOND_EXTRA_RADIUS + radius(atom1));
      }
    }

    return true;
  }
  
  bool StickEngine::renderPick(PainterDevice *pd)
  {
    glDisable( GL_NORMALIZE );
    glEnable( GL_RESCALE_NORMAL );

    // Render the atoms
    foreach(Atom *a, atoms())
      renderPick(pd, a);

    // render bonds (sticks)
    glDisable( GL_RESCALE_NORMAL );
    glEnable( GL_NORMALIZE );
    foreach(Bond *b, bonds())
      renderOpaque(pd, b);

    return true;
  }

  inline bool StickEngine::renderOpaque(PainterDevice *pd, const Atom* a)
  {
    Color *map = colorMap(); // possible custom color map
    if (!map) map = pd->colorMap(); // fall back to global color map

    map->setFromPrimitive(a);
    pd->painter()->setColor(map);
    pd->painter()->setName(a);
    pd->painter()->drawSphere( a->pos(), radius(a) );

    return true;
  }

  inline bool StickEngine::renderPick(PainterDevice *pd, const Atom* a)
  {
    Color *map = colorMap(); // possible custom color map
    if (!map) map = pd->colorMap(); // fall back to global color map

    map->setFromPrimitive(a);
    pd->painter()->setColor(map);
    pd->painter()->setName(a);
    pd->painter()->drawSphere( a->pos(), radius(a) + 0.2 );

    return true;
  }

  inline bool StickEngine::renderOpaque(PainterDevice *pd, const Bond* b)
  {
    Color *map = colorMap(); // possible custom color map
    if (!map) map = pd->colorMap(); // fall back to global color map

    Atom* atom1 = pd->molecule()->atomById(b->beginAtomId());
    Atom* atom2 = pd->molecule()->atomById(b->endAtomId());
    Vector3d v1 (*atom1->pos());
    Vector3d v2 (*atom2->pos());
    Vector3d v3 (( v1 + v2 ) / 2);

    map->setFromPrimitive(atom1);
    pd->painter()->setColor(map);
    pd->painter()->setName(b);
    pd->painter()->drawCylinder( v1, v3, radius(atom1) );

    map->setFromPrimitive(atom2);
    pd->painter()->setColor(map);
    pd->painter()->setName(b);
    pd->painter()->drawCylinder( v3, v2, radius(atom1) );

    return true;
  }

  double StickEngine::radius(const PainterDevice *pd, const Primitive *p) const
  {
    // Atom radius
    if (p->type() == Primitive::AtomType)
    {
      if (pd)
      {
        if (pd->isSelected(p))
          return radius(static_cast<const Atom *>(p)) + SEL_ATOM_EXTRA_RADIUS;
      }
      return radius(static_cast<const Atom *>(p));
    }
    // Bond radius
    else if (p->type() == Primitive::BondType)
    {
      const Atom* a = pd->molecule()->atomById(static_cast<const Bond *>(p)->beginAtomId());
      if (pd)
      {
        if (pd->isSelected(p))
          return radius(a) + SEL_BOND_EXTRA_RADIUS;
      }
      return radius(a);
    }
    // Something else
    else
      return 0.;
  }

  Engine::Layers StickEngine::layers() const
  {
    return Engine::Opaque | Engine::Transparent;
  }

    // **** Settings Widget ***

  void StickEngine::setRadius(int value)
  {
    m_radius = value / SCALING_FACTOR;
    emit changed();
  }

  QWidget* StickEngine::settingsWidget()
  {
    if(!m_settingsWidget)
    {
      m_settingsWidget = new StickSettingsWidget();
      connect(m_settingsWidget->radiusSlider, SIGNAL(valueChanged(int)), this, SLOT(setRadius(int)));
      connect(m_settingsWidget, SIGNAL(destroyed()), this, SLOT(settingsWidgetDestroyed()));
      m_settingsWidget->radiusSlider->setValue(SCALING_FACTOR*m_radius);
    }
    return m_settingsWidget;
  }

  void StickEngine::settingsWidgetDestroyed()
  {
    m_settingsWidget = 0;
  }

  void StickEngine::writeSettings(QSettings &settings) const
  {
    Engine::writeSettings(settings);
    settings.setValue("radius", SCALING_FACTOR*m_radius);
  }

  void StickEngine::readSettings(QSettings &settings)
  {
    Engine::readSettings(settings);
        // default = 0.25 as far as m_radius
    setRadius(settings.value("radius", 5).toInt());
    if (m_settingsWidget) {
      m_settingsWidget->radiusSlider->setValue(SCALING_FACTOR*m_radius);
    }
  }
}

Q_EXPORT_PLUGIN2(stickengine, Avogadro::StickEngineFactory)
