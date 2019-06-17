/**
 * @file
 * @copyright This code is licensed under the 3-clause BSD license.\n
 *            Copyright ETH Zurich, Laboratory for Physical Chemistry, Reiher Group.\n
 *            See LICENSE.txt for details.
 */

/* Internal Includes */
#include "DFTB0MethodWrapper.h"
#include "DFTB0Settings.h"
#include <Sparrow/Implementations/Dftb/Utils/DipoleUtils/DFTBDipoleMatrixCalculator.h>
#include <Sparrow/Implementations/Dftb/Utils/DipoleUtils/DFTBDipoleMomentCalculator.h>
/* External Includes */
#include <Core/Exceptions.h>
#include <Utils/Geometry/AtomCollection.h>
#include <Utils/IO/NativeFilenames.h>
#include <Utils/MethodEssentials/util/MethodExceptions.h>
#include <Utils/UniversalSettings/SettingsNames.h>
#include <memory>

namespace Scine {
namespace Sparrow {

DFTB0MethodWrapper::DFTB0MethodWrapper() {
  this->settings_ = std::make_unique<DFTB0Settings>();
  requiredProperties_ = Utils::Property::Energy;
  dipoleCalculator_ = std::make_unique<DFTBDipoleMomentCalculator<dftb::DFTB0>>(method_);
  dipoleMatrixCalculator_ = DFTBDipoleMatrixCalculator<dftb::DFTB0>::create(method_);
  applySettings();
};

DFTB0MethodWrapper::DFTB0MethodWrapper(const DFTB0MethodWrapper& rhs) : DFTB0MethodWrapper() {
  copyInto(*this, rhs);
}

DFTB0MethodWrapper& DFTB0MethodWrapper::operator=(const DFTB0MethodWrapper& rhs) {
  copyInto(*this, rhs);
  return *this;
}

DFTB0MethodWrapper::~DFTB0MethodWrapper() = default;

void DFTB0MethodWrapper::applySettings() {
  if (settings_->check()) {
    int molecularCharge = settings_->getInt(Utils::SettingsNames::molecularCharge);
    auto logVerbosity = settings_->getString(Utils::SettingsNames::loggerVerbosity);

    method_.setMolecularCharge(molecularCharge);
    method_.startLogger(logVerbosity);
  }
  else {
    throw Core::InitializationException("settings invalid!");
  }
};

std::string DFTB0MethodWrapper::name() const {
  return "DFTB0";
}

void DFTB0MethodWrapper::initialize() {
  try {
    auto parameterFile = settings_->getString(Utils::SettingsNames::parameterFile);
    auto resourceDirectory = settings_->getString(Utils::SettingsNames::parameterRootDirectory);
    auto fullPathToParameters = Utils::NativeFilenames::combinePathSegments(resourceDirectory, parameterFile);
    method_.initializeFromParameterPath(fullPathToParameters);
  }
  catch (Utils::Methods::InitializationException& e) {
    throw Core::InitializationException(e.what());
  }
}

Utils::LCAOMethod& DFTB0MethodWrapper::getLCAOMethod() {
  return method_;
}

const Utils::LCAOMethod& DFTB0MethodWrapper::getLCAOMethod() const {
  return method_;
}

void DFTB0MethodWrapper::calculateImpl(Utils::derivativeType requiredDerivative) {
  method_.calculate(requiredDerivative);
}

Utils::DensityMatrix DFTB0MethodWrapper::getDensityMatrixGuess() const {
  Utils::DensityMatrix densityMatrix;
  densityMatrix.resize(method_.getNumberAtomicOrbitals());
  return densityMatrix;
}

void DFTB0MethodWrapper::copyInto(DFTB0MethodWrapper& instance, const DFTB0MethodWrapper& classToCopy) {
  instance.results() = classToCopy.results();
  instance.settings() = classToCopy.settings();
  // Concurrent calling of the logger introduces race conditions
  // that eventually trigger a segfault
  instance.settings().modifyString(Utils::SettingsNames::loggerVerbosity, Utils::SettingsNames::LogLevels::none);
  instance.setStructure(*classToCopy.getStructure());
}

} /* namespace Sparrow */
} /* namespace Scine */
