/**
 * @file
 * @copyright This code is licensed under the 3-clause BSD license.\n
 *            Copyright ETH Zurich, Laboratory for Physical Chemistry, Reiher Group.\n
 *            See LICENSE.txt for details.
 */

#include "../parameters_location.h"
#include <Core/Interfaces/Calculator.h>
#include <Core/ModuleManager.h>
#include <Sparrow/Implementations/Dftb/Dftb2/DFTB2.h>
#include <Sparrow/Implementations/Dftb/Dftb2/Wrapper/DFTB2MethodWrapper.h>
#include <Utils/Constants.h>
#include <Utils/Geometry/AtomCollection.h>
#include <Utils/Geometry/ElementTypes.h>
#include <Utils/IO/ChemicalFileFormats/XYZStreamHandler.h>
#include <Utils/MethodEssentials/MethodFactories/MixerFactory.h>
#include <gmock/gmock.h>
#include <boost/dll/runtime_symbol_info.hpp>

namespace Scine {
namespace Sparrow {

using namespace testing;
using Utils::derivativeType;
using Utils::scf_mixer_t;

class ADFTB2Calculation : public Test {
 public:
  dftb::DFTB2 method;
  std::shared_ptr<Core::Calculator> dynamicallyLoadedMethodWrapper;
  std::shared_ptr<DFTB2MethodWrapper> calculator;

  void SetUp() override {
    calculator = std::make_shared<DFTB2MethodWrapper>();
    auto& moduleManager = Core::ModuleManager::getInstance();
    auto programPath = boost::dll::program_location();
    auto libPath = programPath.parent_path() / "sparrow";
    try {
      moduleManager.load(libPath);
    }
    catch (const std::runtime_error& e) {
      // Do nothing if module is already loaded.
    }
    dynamicallyLoadedMethodWrapper = moduleManager.get<Core::Calculator>("DFTB2");
    dynamicallyLoadedMethodWrapper->settings().modifyString(Utils::SettingsNames::parameterRootDirectory, "");
    dynamicallyLoadedMethodWrapper->settings().modifyString(Utils::SettingsNames::parameterFile, parameters_mio_1_1);
    calculator->settings().modifyString(Utils::SettingsNames::parameterRootDirectory, "");
    calculator->settings().modifyString(Utils::SettingsNames::parameterFile, parameters_mio_1_1);
  }
};

TEST_F(ADFTB2Calculation, HasTheCorrectNumberOfAtomsAfterInitialization) {
  std::stringstream ss("2\n\n"
                       "C     0.0000000000    0.0000000000   -0.0000000000\n"
                       "H     2.0000000000    0.0000000000    0.0000000000\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  method.setAtomCollection(as);
  method.initializeFromParameterPath(parameters_mio_1_1);
  ASSERT_THAT(method.getNumberAtoms(), Eq(2));
}

TEST_F(ADFTB2Calculation, HasTheCorrectNumberOfOrbitalsAfterInitialization) {
  std::stringstream ss("2\n\n"
                       "C     0.0000000000    0.0000000000   -0.0000000000\n"
                       "H     2.0000000000    0.0000000000    0.0000000000\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  method.setAtomCollection(as);
  method.initializeFromParameterPath(parameters_mio_1_1);
  ASSERT_THAT(method.getNumberAtomicOrbitals(), Eq(5));
}

TEST_F(ADFTB2Calculation, GetsSameResultAsDFTBPlusForC) {
  std::stringstream ss("1\n\n"
                       "C     2.0000000000    0.0000000000    0.0000000000\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  method.setAtomCollection(as);
  method.initializeFromParameterPath(parameters_mio_1_1);
  method.calculate(derivativeType::first);

  // Check number of atoms and orbitals
  ASSERT_THAT(method.getNumberAtoms(), Eq(1));
  ASSERT_THAT(method.getNumberAtomicOrbitals(), Eq(4));

  // Check energy
  ASSERT_THAT(method.getEnergy(), DoubleNear(-1.3984936602, 1e-8));

  // Check eigenvalues
  auto eigenvalues = method.getSingleParticleEnergies().getRestrictedEnergies();
  std::vector<double> expected = {-0.50489172, -0.19435511, -0.19435511, -0.19435511};
  for (unsigned i = 0; i < eigenvalues.size(); i++) {
    SCOPED_TRACE("... for the eigenvalue " + std::to_string(i) + ":");
    EXPECT_THAT(eigenvalues[i], DoubleNear(expected[i], 1e-10));
  }

  // Check force
  ASSERT_THAT(method.getGradients().row(0).norm(), DoubleEq(0));
}

TEST_F(ADFTB2Calculation, GetsSameResultAsDFTBPlusForCH4) {
  std::stringstream ss("5\n\n"
                       "C      0.0000000000    0.0000000000    0.0000000000\n"
                       "H      0.6287000000    0.6287000000    0.6287000000\n"
                       "H     -0.6287000000   -0.6287000000    0.6287000000\n"
                       "H     -0.6287000000    0.6287000000   -0.6287000000\n"
                       "H      0.6287000000   -0.6287000000   -0.6287000000\n");
  auto as = Utils::XYZStreamHandler::read(ss);

  method.setScfMixer(scf_mixer_t::fock_diis);
  method.setMaxIterations(10000);
  method.setConvergenceCriteria(1e-8);

  method.setAtomCollection(as);
  method.initializeFromParameterPath(parameters_mio_1_1);
  method.calculate(derivativeType::first);

  // Check number of atoms and orbitals
  ASSERT_THAT(method.getNumberAtoms(), Eq(5));
  ASSERT_THAT(method.getNumberAtomicOrbitals(), Eq(8));

  // Check energy
  ASSERT_THAT(method.getEnergy(), DoubleNear(-3.2256725551, 1e-10));

  // Check atomic charges
  std::vector<double> expectedCharges = {-0.30575605, 0.07643901, 0.07643901, 0.07643901};
  for (unsigned long long i = 0; i < expectedCharges.size(); i++) {
    SCOPED_TRACE("... for the charge " + std::to_string(i) + ":");
    EXPECT_THAT(method.getAtomicCharges()[i], DoubleNear(expectedCharges[i], 1e-6));
  }

  // Check eigenvalues
  auto eigenvalues = method.getSingleParticleEnergies().getRestrictedEnergies();
  std::vector<double> expected = {-0.57457347, -0.33599573, -0.33599573, -0.33599573,
                                  0.35138661,  0.35138661,  0.35138661,  0.59576438};

  for (unsigned i = 0; i < eigenvalues.size(); i++) {
    SCOPED_TRACE("... for the eigenvalue " + std::to_string(i) + ":");
    EXPECT_THAT(eigenvalues[i], DoubleNear(expected[i], 1e-6));
  }

  // Check force
  Eigen::RowVector3d f0(2.905661822261152E-017, 4.770489558936220E-017, 4.943961906533900E-017);
  Eigen::RowVector3d f1(-4.131279691482748E-005, -4.131279691484135E-005, -4.131279691485523E-005);
  Eigen::RowVector3d f2(4.131279691484829E-005, 4.131279691477197E-005, -4.131279691486217E-005);
  Eigen::RowVector3d f3(4.131279691477197E-005, -4.131279691478584E-005, 4.131279691480666E-005);
  Eigen::RowVector3d f4(-4.131279691484135E-005, 4.131279691482054E-005, 4.131279691486217E-005);

  ASSERT_TRUE((-method.getGradients().row(0) - (f0)).norm() < 1e-5);
  ASSERT_TRUE((-method.getGradients().row(1) - (f1)).norm() < 1e-5);
  ASSERT_TRUE((-method.getGradients().row(2) - (f2)).norm() < 1e-5);
  ASSERT_TRUE((-method.getGradients().row(3) - (f3)).norm() < 1e-5);
  ASSERT_TRUE((-method.getGradients().row(4) - (f4)).norm() < 1e-5);
}

TEST_F(ADFTB2Calculation, GetsSameResultAsDFTBPlusForUnrestrictedCH3) {
  std::stringstream ss("4\n\n"
                       "C      0.0000000000    0.0000000000    0.0000000000\n"
                       "H      0.6287000000    0.6287000000    0.6287000000\n"
                       "H     -0.6287000000   -0.6287000000    0.6287000000\n"
                       "H     -0.6287000000    0.6287000000   -0.6287000000\n");
  auto as = Utils::XYZStreamHandler::read(ss);

  method.setScfMixer(scf_mixer_t::fock_diis);
  method.setMaxIterations(10000);
  method.setConvergenceCriteria(1e-8);

  method.setAtomCollection(as);
  method.initializeFromParameterPath(parameters_mio_1_1);
  method.setUnrestrictedCalculation(true);
  method.setSpinMultiplicity(2);
  method.calculate(derivativeType::first);

  // Check number of atoms and orbitals
  ASSERT_THAT(method.getNumberAtoms(), Eq(4));
  ASSERT_THAT(method.getNumberAtomicOrbitals(), Eq(7));

  // Check energy
  ASSERT_THAT(method.getEnergy(), DoubleNear(-2.7458925732, 1e-8));

  // Check atomic charges
  std::vector<double> expectedCharges = {-0.29151134, 0.09717045, 0.09717045, 0.09717045};
  for (unsigned long long i = 0; i < expectedCharges.size(); i++) {
    SCOPED_TRACE("... for the charge " + std::to_string(i) + ":");
    EXPECT_THAT(method.getAtomicCharges()[i], DoubleNear(expectedCharges[i], 1e-6));
  }

  // Check eigenvalues
  auto alphaEigenvalues = method.getSingleParticleEnergies().getAlphaEnergies();
  auto betaEigenvalues = method.getSingleParticleEnergies().getBetaEnergies();
  std::vector<double> alphaExpected = {-0.58373525, -0.35004087, -0.35004087, -0.23077050,
                                       0.33809319,  0.33809319,  0.51658701};
  std::vector<double> betaExpected = {-0.54684410, -0.32740014, -0.32740014, -0.18248896,
                                      0.36091443,  0.36091443,  0.53408108};

  for (unsigned i = 0; i < alphaEigenvalues.size(); i++) {
    SCOPED_TRACE("... for the eigenvalue " + std::to_string(i) + ":");
    EXPECT_THAT(alphaEigenvalues[i], DoubleNear(alphaExpected[i], 1e-6));
    EXPECT_THAT(betaEigenvalues[i], DoubleNear(betaExpected[i], 1e-6));
  }

  // Check force
  Eigen::RowVector3d f0(-1.738173073648139E-002, 1.738173073648139E-002, 1.738173073648149E-002);
  Eigen::RowVector3d f1(1.136823505565547E-002, -3.006747840412945E-003, -3.006747840413014E-003);
  Eigen::RowVector3d f2(3.006747840413014E-003, -1.136823505565546E-002, -3.006747840412952E-003);
  Eigen::RowVector3d f3(3.006747840412931E-003, -3.006747840412986E-003, -1.136823505565552E-002);

  ASSERT_TRUE((-method.getGradients().row(0) - (f0)).norm() < 1e-5);
  ASSERT_TRUE((-method.getGradients().row(1) - (f1)).norm() < 1e-5);
  ASSERT_TRUE((-method.getGradients().row(2) - (f2)).norm() < 1e-5);
  ASSERT_TRUE((-method.getGradients().row(3) - (f3)).norm() < 1e-5);
}

TEST_F(ADFTB2Calculation, MethodWrapperCanBeCloned) {
  dynamicallyLoadedMethodWrapper->settings().modifyInt(Utils::SettingsNames::molecularCharge, 2);

  std::stringstream ss("9\n\n"
                       "H      1.9655905060   -0.0263662325    1.0690084915\n"
                       "C      1.3088788172   -0.0403821764    0.1943189946\n"
                       "H      1.5790293586    0.8034866305   -0.4554748131\n"
                       "H      1.5186511399   -0.9518066799   -0.3824432806\n"
                       "C     -0.1561112248    0.0249676675    0.5877379610\n"
                       "H     -0.4682794700   -0.8500294693    1.1854276282\n"
                       "H     -0.4063173598    0.9562730342    1.1264955766\n"
                       "O     -0.8772416674    0.0083263307   -0.6652828084\n"
                       "H     -1.8356000997    0.0539308952   -0.5014877498\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  dynamicallyLoadedMethodWrapper->setStructure(as);

  auto cloned = dynamicallyLoadedMethodWrapper->clone();

  auto mc = Utils::SettingsNames::molecularCharge;
  ASSERT_EQ(cloned->settings().getInt(mc), dynamicallyLoadedMethodWrapper->settings().getInt(mc));
}

TEST_F(ADFTB2Calculation, StructureIsCorrectlyCloned) {
  std::stringstream ss("9\n\n"
                       "H      1.9655905060   -0.0263662325    1.0690084915\n"
                       "C      1.3088788172   -0.0403821764    0.1943189946\n"
                       "H      1.5790293586    0.8034866305   -0.4554748131\n"
                       "H      1.5186511399   -0.9518066799   -0.3824432806\n"
                       "C     -0.1561112248    0.0249676675    0.5877379610\n"
                       "H     -0.4682794700   -0.8500294693    1.1854276282\n"
                       "H     -0.4063173598    0.9562730342    1.1264955766\n"
                       "O     -0.8772416674    0.0083263307   -0.6652828084\n"
                       "H     -1.8356000997    0.0539308952   -0.5014877498\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  dynamicallyLoadedMethodWrapper->setStructure(as);

  auto cloned = dynamicallyLoadedMethodWrapper->clone();

  ASSERT_EQ(cloned->getPositions(), dynamicallyLoadedMethodWrapper->getPositions());
}

TEST_F(ADFTB2Calculation, ClonedMethodCanCalculate) {
  std::stringstream ss("9\n\n"
                       "H      1.9655905060   -0.0263662325    1.0690084915\n"
                       "C      1.3088788172   -0.0403821764    0.1943189946\n"
                       "H      1.5790293586    0.8034866305   -0.4554748131\n"
                       "H      1.5186511399   -0.9518066799   -0.3824432806\n"
                       "C     -0.1561112248    0.0249676675    0.5877379610\n"
                       "H     -0.4682794700   -0.8500294693    1.1854276282\n"
                       "H     -0.4063173598    0.9562730342    1.1264955766\n"
                       "O     -0.8772416674    0.0083263307   -0.6652828084\n"
                       "H     -1.8356000997    0.0539308952   -0.5014877498\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  dynamicallyLoadedMethodWrapper->setStructure(as);

  auto cloned = dynamicallyLoadedMethodWrapper->clone();

  auto resultCloned = cloned->calculate("");
  auto result = dynamicallyLoadedMethodWrapper->calculate("");
  ASSERT_THAT(resultCloned.getEnergy(), DoubleNear(result.getEnergy(), 1e-9));
}

TEST_F(ADFTB2Calculation, ClonedMethodCanCalculateGradients) {
  std::stringstream ss("9\n\n"
                       "H      1.9655905060   -0.0263662325    1.0690084915\n"
                       "C      1.3088788172   -0.0403821764    0.1943189946\n"
                       "H      1.5790293586    0.8034866305   -0.4554748131\n"
                       "H      1.5186511399   -0.9518066799   -0.3824432806\n"
                       "C     -0.1561112248    0.0249676675    0.5877379610\n"
                       "H     -0.4682794700   -0.8500294693    1.1854276282\n"
                       "H     -0.4063173598    0.9562730342    1.1264955766\n"
                       "O     -0.8772416674    0.0083263307   -0.6652828084\n"
                       "H     -1.8356000997    0.0539308952   -0.5014877498\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  dynamicallyLoadedMethodWrapper->setStructure(as);

  auto cloned = dynamicallyLoadedMethodWrapper->clone();

  dynamicallyLoadedMethodWrapper->setRequiredProperties(Utils::Property::Gradients);
  cloned->setRequiredProperties(Utils::Property::Gradients);

  auto resultCloned = cloned->calculate("");
  auto result = dynamicallyLoadedMethodWrapper->calculate("");

  for (int atom = 0; atom < cloned->getPositions().rows(); ++atom) {
    for (int dimension = 0; dimension < 3; ++dimension) {
      ASSERT_THAT(resultCloned.getGradients().row(atom)(dimension),
                  DoubleNear(result.getGradients().row(atom)(dimension), 3e-7));
    }
  }
}

TEST_F(ADFTB2Calculation, ClonedMethodCopiesResultsCorrectly) {
  std::stringstream ss("9\n\n"
                       "H      1.9655905060   -0.0263662325    1.0690084915\n"
                       "C      1.3088788172   -0.0403821764    0.1943189946\n"
                       "H      1.5790293586    0.8034866305   -0.4554748131\n"
                       "H      1.5186511399   -0.9518066799   -0.3824432806\n"
                       "C     -0.1561112248    0.0249676675    0.5877379610\n"
                       "H     -0.4682794700   -0.8500294693    1.1854276282\n"
                       "H     -0.4063173598    0.9562730342    1.1264955766\n"
                       "O     -0.8772416674    0.0083263307   -0.6652828084\n"
                       "H     -1.8356000997    0.0539308952   -0.5014877498\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  dynamicallyLoadedMethodWrapper->setStructure(as);

  auto result = dynamicallyLoadedMethodWrapper->calculate();
  auto cloned = dynamicallyLoadedMethodWrapper->clone();

  ASSERT_THAT(cloned->results().getEnergy(), DoubleNear(result.getEnergy(), 1e-9));
}

/*
TEST_F(ADFTB2Calculation, GetsSameResultAsDFTBPlusForCO) {
  std::stringstream ss("2\n\n"
                         "C      0.0000000000    0.0000000000    0.0000000000\n"
                         "O      0.6287000000    0.6287000000    0.6287000000\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  method.setAtomCollection(as);
  method.initialize(parameters_mio_1_1);
  method.calculate(1);

  //Check number of atoms and orbitals
  ASSERT_THAT(method.getNumberAtoms(), Eq(2));
  ASSERT_THAT(method.getnAOs(), Eq(8));

  //Check energy
  ASSERT_THAT(method.getEnergy(), DoubleNear(-5.0281100118, 1e-10));

  //Check eigenvalues
  auto eigenvalues = method.getRestrictedEigenvalues();
  std::vector<double> expected = { -0.96215242, -0.50129687, -0.41598843, -0.41598843, -0.33713518, -0.01580125,
-0.01580125,  2.01165147 };


  for(long long i=0; i<eigenvalues.size(); i++) {
    SCOPED_TRACE("... for the eigenvalue " + std::to_string(i) + ":");
    EXPECT_THAT(eigenvalues[i], DoubleNear(expected[i],1e-6));
  }

  //Check force
  Eigen::Vector3d f0 (-2.099482951227899E-002, -2.099482951228265E-002, -2.099482951227699E-002);
  Eigen::Vector3d f1 (2.099482951227899E-002,  2.099482951228265E-002,  2.099482951227699E-002);

  ASSERT_TRUE((method.getGradients()[0]-(-f0)).norm()<1e-4);
  ASSERT_TRUE((method.getGradients()[1]-(-f1)).norm()<1e-4);
}

TEST_F(ADFTB2Calculation, GetsSameResultAsDFTBPlusForH2) {
  std::stringstream ss("2\n\n"
                         "H      0.0000000000    0.0000000000    0.0000000000\n"
                         "H      0.6287000000    0.6287000000    0.6287000000\n");
  auto as = Utils::XYZStreamHandler::read(ss);
  method.setAtomCollection(as);
  method.initialize(parameters_mio_1_1);
  method.calculate(1);

  //Check number of atoms and orbitals
  ASSERT_THAT(method.getNumberAtoms(), Eq(2));
  ASSERT_THAT(method.getNumberAtomicOrbitals(), Eq(2));

  //Check energy
  ASSERT_THAT(method.getEnergy(), DoubleNear(-0.6389548092, 1e-10));

  //Check eigenvalues
  auto eigenvalues = method.getRestrictedEigenvalues();
  std::vector<double> expected = { -0.31947740, -0.04784076 };


  for(long long i=0; i<eigenvalues.size(); i++) {
    SCOPED_TRACE("... for the eigenvalue " + std::to_string(i) + ":");
    EXPECT_THAT(eigenvalues[i], DoubleNear(expected[i],1e-6));
  }

  //Check force
  Eigen::Vector3d f0 (3.390660659605971E-002,  3.390660659605971E-002,  3.390660659605971E-002);
  Eigen::Vector3d f1 (-3.390660659605971E-002, -3.390660659605971E-002, -3.390660659605971E-002);

  ASSERT_TRUE((method.getGradients()[0]-(-f0)).norm()<1e-5);
  ASSERT_TRUE((method.getGradients()[1]-(-f1)).norm()<1e-5);
}
*/

TEST_F(ADFTB2Calculation, AtomCollectionCanBeReturned) {
  std::stringstream ssH("4\n\n"
                        "C      0.0000000000    0.0000000000    0.0000000000\n"
                        "C      0.0529177211   -0.3175063264    0.2645886053\n"
                        "H     -0.5291772107    0.1058354421   -0.1587531632\n"
                        "H     -0.1058354421    0.1058354421   -0.1587531632\n");
  auto structure = Utils::XYZStreamHandler::read(ssH);
  calculator->setStructure(structure);
  ASSERT_EQ(structure.getPositions(), calculator->getStructure()->getPositions());
  for (int i = 0; i < structure.getElements().size(); ++i)
    ASSERT_EQ(structure.getElements()[i], calculator->getStructure()->getElements()[i]);
}
} // namespace Sparrow
} // namespace Scine
