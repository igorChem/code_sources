
#ifndef __GABEDIT_NWCHEMGLOBAL_H__
#define __GABEDIT_NWCHEMGLOBAL_H__

GtkWidget* nwchemWin;
NWChemMolecule nwchemMolecule;
NWChemColorFore nwchemColorFore;
NWChemColorBack nwchemColorBack;
NWChemBasis nwchemBasis;
NWChemGuessWaveFunction nwchemGuessWaveFunction;

NWChemFunctional* functionals;
NWChemStdFunctional* stdFunctionals;
gdouble* sumFunctionals;

#endif /* __GABEDIT_NWCHEMGLOBAL_H__ */

