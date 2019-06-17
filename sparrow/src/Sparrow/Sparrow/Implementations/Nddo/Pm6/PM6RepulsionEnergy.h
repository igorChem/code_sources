/**
 * @file
 * @copyright This code is licensed under the 3-clause BSD license.\n
 *            Copyright ETH Zurich, Laboratory for Physical Chemistry, Reiher Group.\n
 *            See LICENSE.txt for details.
 */

#ifndef SPARROW_PM6REPULSIONENERGY_H
#define SPARROW_PM6REPULSIONENERGY_H

#include <Sparrow/Implementations/Nddo/Pm6/PM6PairwiseRepulsion.h>
#include <Utils/MethodEssentials/Methods/RepulsionCalculator.h>
#include <Utils/Typenames.h>
#include <memory>
#include <vector>

namespace Scine {

namespace Utils {
enum class derivOrder;
}

namespace Sparrow {
namespace nddo {
class ElementParameters;
class ElementPairParameters;

/**
 * @brief This class sums up the core-core repulsion energies and the corresponding derivatives with respect to
 *        the nuclear cartesian coordinate between all pairs of cores.
 * It inherits from Utils::RepulsionCalculator in order for it to work with the LCAO/SCFMethod polymorphic system.
 */
class PM6RepulsionEnergy : public Utils::RepulsionCalculator {
 public:
  using pairRepulsion_t = std::unique_ptr<PM6PairwiseRepulsion>;
  using Container = std::vector<std::vector<pairRepulsion_t>>;

  //! @brief Constructor.
  PM6RepulsionEnergy(const Utils::ElementTypeCollection& elements, const Utils::PositionCollection& positions,
                     const ElementParameters& elementParameters, const ElementPairParameters& pairParameters);
  //! @brief Overrides virtual base class desctructor with default implementation.
  ~PM6RepulsionEnergy() override;

  //! @brief Initializes the core-core repulsion pairs
  void initialize() override;

  //! @brief Starts the calculation of the core-core repulsion up to the \param order derivative order.
  void calculateRepulsion(Utils::derivOrder order) override;
  //! @brief Sums up all the single core-core contributions to return the overall core-core repulsion energy.
  double getRepulsionEnergy() const override;
  //! Functions calculating the core-core derivative contributions up to the corresponding derivative order.
  void addRepulsionDerivatives(
      Utils::AutomaticDifferentiation::DerivativeContainerType<Utils::derivativeType::first>& derivatives) const override;
  void addRepulsionDerivatives(
      Utils::AutomaticDifferentiation::DerivativeContainerType<Utils::derivativeType::second_atomic>& derivatives) const override;
  void addRepulsionDerivatives(
      Utils::AutomaticDifferentiation::DerivativeContainerType<Utils::derivativeType::second_full>& derivatives) const override;

 private:
  template<Utils::derivativeType O>
  void addRepulsionDerivativesImpl(Utils::AutomaticDifferentiation::DerivativeContainerType<O>& derivatives) const;

  void calculatePairRepulsion(int i, int j, Utils::derivOrder order);
  void initializePair(int i, int j);

  const ElementParameters& elementParameters_;
  const ElementPairParameters& pairParameters_;
  Container rep_;
  int nAtoms_;
  const Utils::ElementTypeCollection& elementTypes_;
  const Utils::PositionCollection& positions_;
};

} // namespace nddo

} // namespace Sparrow
} // namespace Scine
#endif // SPARROW_REPULSIONENERGY_H
