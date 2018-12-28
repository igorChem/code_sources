/**********************************************************************
  EngineListView - View for listing engines

  Copyright (C) 2007 by Geoffrey R. Hutchison

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

#include "enginelistview.h"

#include "engineitemmodel.h"

#include <avogadro/engine.h>
#include <avogadro/glwidget.h>

#include <QStandardItemModel>
#include <QAbstractButton>
#include <QStandardItem>
#include <QVBoxLayout>
#include <QDialog>
#include <QSortFilterProxyModel>

namespace Avogadro {

  EngineListView::EngineListView( GLWidget *glWidget, QWidget *parent ) : 
    QListView(parent), m_glWidget(glWidget)
  {
    m_model = new EngineItemModel(m_glWidget, this);

    // This should sort the engine names for user views
    // It should also update dynamically as people edit names
    // Somehow it doesn't work right from the start!
    QSortFilterProxyModel *sortModel = new QSortFilterProxyModel(this);
    sortModel->setSourceModel(m_model);
    setModel(sortModel);
    sortModel->setSortCaseSensitivity(Qt::CaseInsensitive);
    sortModel->setSortLocaleAware(true);
    sortModel->setDynamicSortFilter(true);
    sortModel->sort(0, Qt::AscendingOrder);
		
    connect(this, SIGNAL(clicked(QModelIndex)),
        this, SLOT(selectEngine(QModelIndex)));
		// This might work for having the proxy model emit the signal, but let's keep it as-is
    connect(m_model, SIGNAL(dataChanged(QModelIndex, QModelIndex)),
        glWidget, SLOT(update()));
        
    // improves display performance
    setUniformItemSizes(true);
    setAlternatingRowColors(true); // looks better
  }

  EngineListView::~EngineListView()
  {
  }

  GLWidget *EngineListView::glWidget() const
  {
    return m_glWidget;
  }

  void EngineListView::selectEngine( const QModelIndex &index )
  {
    Engine *engine = model()->data(index, EngineItemModel::EngineRole).value<Engine *>();
    if(engine) {
      emit clicked(engine);
    }
  }

  Engine *EngineListView::selectedEngine() const
  {
    QModelIndex idx = currentIndex();
    if(idx.isValid())
    {
      Engine *engine = model()->data(idx, EngineItemModel::EngineRole).value<Engine *>();
      return engine;
    }

    return 0;
  }
  
  void EngineListView::clear()
  {
    m_model->clear();
  }

} // end namespace Avogadro

#include "enginelistview.moc"
