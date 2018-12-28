import Avogadro
import unittest
from PyQt4.QtGui import *
from numpy import *
from util import *

class TestMesh(unittest.TestCase):
  def setUp(self):
    self.molecule = Avogadro.molecules.addMolecule()
    self.mesh = self.molecule.addMesh()

  def test_name(self):
    self.mesh.name = "testing"
    self.assertEqual(self.mesh.name, "testing")
 
  def test_reserve(self):
    # just check if the method is there
    self.assertEqual(self.mesh.reserve(100, True), True)

  def test_various(self):
    self.mesh.otherMesh
    self.mesh.cube

    self.assertEqual(self.mesh.numVertices, 0)
    self.assertEqual(self.mesh.numNormals, 0)
  
  def test_stable(self):
    self.mesh.stable = False
    self.assertEqual(self.mesh.stable, False)
    self.mesh.stable = True
    self.assertEqual(self.mesh.stable, True)

  def test_isoValue(self):
    testReadWriteProperty(self, self.mesh.isoValue, 0.0, 3.0)

  def test_vertices(self):
    vertices = []
    vertices.append(array([0., 0., 1.]))
    vertices.append(array([1., 0., 0.]))
    vertices.append(array([0., 1., 0.]))

    self.mesh.vertices = vertices
    self.assertEqual(len(self.mesh.vertices), 3)
    self.assertEqual(self.mesh.vertex(0)[2], 1)
    
    self.mesh.addVertices(vertices)
    self.assertEqual(len(self.mesh.vertices), 6)
 
  def test_normals(self):
    normals = []
    normals.append(array([0., 0., 1.]))
    normals.append(array([1., 0., 0.]))
    normals.append(array([0., 1., 0.]))

    self.mesh.normals = normals
    self.assertEqual(len(self.mesh.normals), 3)
    self.assertEqual(self.mesh.normal(0)[2], 1)
 
    self.mesh.addNormals(normals)
    self.assertEqual(len(self.mesh.normals), 6)
 
  def test_colors(self):
    colors = []
    colors.append(QColor(255,0,0))
    colors.append(QColor(255,0,0))
    colors.append(QColor(255,0,0))

    self.mesh.colors = colors
    self.assertEqual(len(self.mesh.colors), 3)
    self.assertEqual(self.mesh.color(0).red(), 255)
 
    self.mesh.addColors(colors)
    self.assertEqual(len(self.mesh.colors), 6)
  
  def test_valid(self):
    # just test it's there...
    self.assertEqual(self.mesh.valid, True)
 
  def test_normals(self):
    l = []
    l.append(array([0., 0., 1.]))
    l.append(array([1., 0., 0.]))
    l.append(array([0., 1., 0.]))

    self.mesh.vertices = l
    self.mesh.normals = l
    
    self.mesh.clear()
    self.assertEqual(len(self.mesh.vertices), 0)
    self.assertEqual(len(self.mesh.normals), 0)
  

if __name__ == "__main__":
  unittest.main()
