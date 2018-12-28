/**********************************************************************
  RingEngine - Engine for "ring" display

  Copyright (C) 2007 by Marcus D. Hanwell

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

#include "ringengine.h"

#include <avogadro/color.h>
#include <avogadro/glwidget.h>
#include <avogadro/camera.h>
#include <avogadro/painterdevice.h>
#include <avogadro/molecule.h>
#include <avogadro/fragment.h>
#include <avogadro/atom.h>

#include <QMessageBox>
#include <QString>
#include <QDebug>

using namespace std;
using namespace Eigen;

namespace Avogadro{

  const float ringColors[6][3] = {
    { 1.0, 0.0, 0.0 },
    { 0.0, 1.0, 0.0 },
    { 0.0, 0.0, 1.0 },
    { 1.0, 0.0, 1.0 },
    { 1.0, 1.0, 0.0 },
    { 0.0, 1.0, 1.0 }
  };

  RingEngine::RingEngine(QObject *parent) : Engine(parent), m_settingsWidget(0),
    m_alpha(1.0)
  {
  }

  Engine *RingEngine::clone() const
  {
    RingEngine *engine = new RingEngine(parent());
    engine->setAlias(alias());
    engine->m_alpha = m_alpha;
    engine->setEnabled(isEnabled());

    return engine;
  }

  RingEngine::~RingEngine()
  {
  }

  bool RingEngine::renderOpaque(PainterDevice *pd)
  {
    if (m_alpha < 0.999) return true;

    // Special case for everything up to 7 membered rings.
    QList<Fragment *> rings = const_cast<Molecule *>(pd->molecule())->rings();
    // Now actually draw the ring structures
    foreach(Fragment *r, rings)
      renderRing(r->atoms(), pd);

    return true;
  }

  bool RingEngine::renderTransparent(PainterDevice *pd)
  {
    if (m_alpha > 0.999) return true;

    // Special case for everything up to 7 membered rings.
    QList<Fragment *> rings = const_cast<Molecule *>(pd->molecule())->rings();
    // Now actually draw the ring structures
    foreach(Fragment *r, rings)
      renderRing(r->atoms(), pd);

    return true;
  }

  bool RingEngine::renderRing(const QList<unsigned long> &ring, PainterDevice *pd)
  {
    // We need to get rid of the constness in order to get the atoms
    Molecule *mol = const_cast<Molecule *>(pd->molecule());

    // Calculate an appropriate normal and use it for all the triangles in the
    // ring - this will give consistent lighting.
    Eigen::Vector3d v1, v2, norm;
    v1 = *mol->atomById(ring[1])->pos() - *mol->atomById(ring[0])->pos();
    v2 = *mol->atomById(ring[2])->pos() - *mol->atomById(ring[1])->pos();
    norm = v1.cross(v2);
    if (norm.dot(pd->camera()->backTransformedZAxis()) > 0) norm *= -1;

    // Disable face culling for ring structures.
    glDisable(GL_CULL_FACE);

    // Optimize for smaller ring structures
    switch (ring.size()) {
      case 3:
        // Single triangle - easy
        pd->painter()->setColor(ringColors[0][0], ringColors[0][1],
                                ringColors[0][2], m_alpha);
        pd->painter()->drawTriangle(*mol->atomById(ring[0])->pos(),
                                    *mol->atomById(ring[1])->pos(),
                                    *mol->atomById(ring[2])->pos(),
                                    norm);
        break;
      case 4:
        // Two triangles
        pd->painter()->setColor(ringColors[1][0], ringColors[1][1],
                                ringColors[1][2], m_alpha);
        pd->painter()->drawTriangle(*mol->atomById(ring[0])->pos(),
                                    *mol->atomById(ring[1])->pos(),
                                    *mol->atomById(ring[2])->pos(),
                                    norm);
        pd->painter()->drawTriangle(*mol->atomById(ring[0])->pos(),
                                    *mol->atomById(ring[2])->pos(),
                                    *mol->atomById(ring[3])->pos(),
                                    norm);
        break;
      case 5:
        // Three triangles
        pd->painter()->setColor(ringColors[2][0], ringColors[2][1],
                                ringColors[2][2], m_alpha);
        pd->painter()->drawTriangle(*mol->atomById(ring[0])->pos(),
                                    *mol->atomById(ring[1])->pos(),
                                    *mol->atomById(ring[2])->pos(),
                                    norm);
        pd->painter()->drawTriangle(*mol->atomById(ring[0])->pos(),
                                    *mol->atomById(ring[2])->pos(),
                                    *mol->atomById(ring[3])->pos(),
                                    norm);
        pd->painter()->drawTriangle(*mol->atomById(ring[0])->pos(),
                                    *mol->atomById(ring[3])->pos(),
                                    *mol->atomById(ring[4])->pos(),
                                    norm);
        break;
      case 6:
        // Four triangles
        pd->painter()->setColor(ringColors[3][0], ringColors[3][1],
                                ringColors[3][2], m_alpha);
        pd->painter()->drawTriangle(*mol->atomById(ring[0])->pos(),
                                    *mol->atomById(ring[1])->pos(),
                                    *mol->atomById(ring[2])->pos(),
                                    norm);
        pd->painter()->drawTriangle(*mol->atomById(ring[2])->pos(),
                                    *mol->atomById(ring[3])->pos(),
                                    *mol->atomById(ring[4])->pos(),
                                    norm);
        pd->painter()->drawTriangle(*mol->atomById(ring[4])->pos(),
                                    *mol->atomById(ring[5])->pos(),
                                    *mol->atomById(ring[0])->pos(),
                                    norm);
        pd->painter()->drawTriangle(*mol->atomById(ring[0])->pos(),
                                    *mol->atomById(ring[2])->pos(),
                                    *mol->atomById(ring[4])->pos(),
                                    norm);
        break;
      default:
        // The generic case - find the centre of the ring and draw a triangle fan
        pd->painter()->setColor(ringColors[4][0], ringColors[4][1],
                                ringColors[4][2], m_alpha);
        Vector3d center;
        for (int i = 0; i < ring.size(); i++)
          center += *mol->atomById(ring[i])->pos();
        center /= ring.size();
        for (int i = 0; i < ring.size()-1; i++)
          pd->painter()->drawTriangle(center,
                                      *mol->atomById(ring[i])->pos(),
                                      *mol->atomById(ring[i+1])->pos(),
                                      norm);
        pd->painter()->drawTriangle(center,
                                    *mol->atomById(ring[ring.size()-1])->pos(),
                                    *mol->atomById(ring[0])->pos(),
                                    norm);

    }
    return true;
  }

  double RingEngine::radius(const PainterDevice *, const Primitive *) const
  {
    return 0.;
  }

  double RingEngine::transparencyDepth() const
  {
    return 1.0;
  }

  Engine::Layers RingEngine::layers() const
  {
    return Engine::Opaque | Engine::Transparent;
  }

  Engine::PrimitiveTypes RingEngine::primitiveTypes() const
  {
    return Engine::Fragments;
  }

  Engine::ColorTypes RingEngine::colorTypes() const
  {
    return Engine::IndexedColors;
  }

  void RingEngine::setOpacity(int value)
  {
    m_alpha = 0.05 * value;
    emit changed();
  }

  QWidget* RingEngine::settingsWidget()
  {
    if(!m_settingsWidget)
    {
      m_settingsWidget = new RingSettingsWidget();
      connect(m_settingsWidget->opacitySlider, SIGNAL(valueChanged(int)),
              this, SLOT(setOpacity(int)));
      connect(m_settingsWidget, SIGNAL(destroyed()),
              this, SLOT(settingsWidgetDestroyed()));
      m_settingsWidget->opacitySlider->setValue(int(20*m_alpha));
    }
    return m_settingsWidget;
  }

  void RingEngine::settingsWidgetDestroyed()
  {
    qDebug() << "Destroyed Settings Widget";
    m_settingsWidget = 0;
  }

  void RingEngine::writeSettings(QSettings &settings) const
  {
    Engine::writeSettings(settings);
    settings.setValue("opacity", 20*m_alpha);
  }

  void RingEngine::readSettings(QSettings &settings)
  {
    Engine::readSettings(settings);
    setOpacity(settings.value("opacity", 20).toInt());
    if (m_settingsWidget) {
      m_settingsWidget->opacitySlider->setValue(int(20*m_alpha));
    }
  }


}

Q_EXPORT_PLUGIN2(ringengine, Avogadro::RingEngineFactory)
