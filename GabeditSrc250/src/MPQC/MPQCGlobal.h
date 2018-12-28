
#ifndef __GABEDIT_MPQCGLOBAL_H__
#define __GABEDIT_MPQCGLOBAL_H__

GtkWidget* mpqcWin;
MPQCMolecule mpqcMolecule;
MPQCColorFore mpqcColorFore;
MPQCColorBack mpqcColorBack;
MPQCMole mpqcMole;
MPQCGuessWaveFunction mpqcGuessWaveFunction;
MPQCMpqc mpqcMpqc;
MPQCBasis mpqcBasis;

MPQCFunctional* functionals;
MPQCStdFunctional* stdFunctionals;
gdouble* sumFunctionals;
MPQCOptimization mpqcOptimization;

#endif /* __GABEDIT_MPQCGLOBAL_H__ */

