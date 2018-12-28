OBJECTS = EnergiesCurves.o Fragments.o GeomConversion.o GeomXYZ.o GeomZmatrix.o ImagesGeom.o InterfaceGeom.o Measure.o Povray.o ResultsAnalise.o GeomSymmetry.o RotFragments.o BuildLinear.o BuildRing.o BuildRoZPhi.o BuildPolyPeptide.o FragmentsPPD.o FragmentsPSC.o FragmentsPNA.o BuildPolySaccharide.o BuildPolyNucleicAcid.o BuildNanoTube.o BuildPeriodic.o PersonalFragments.o SavePersonalFragments.o LoadPersonalFragments.o FragmentsSelector.o OpenBabel.o MenuToolBarGeom.o PreviewGeom.o FragmentsTree.o ExportGeom.o TreeMolecule.o DrawGeomGL.o  AxesGeomGL.o DrawGeomCairo.o SelectionDlg.o

include ../../CONFIG
CFLAGS =  $(COMMONCFLAGS) $(GTKCFLAGS)

all: compiling 

%.o : %.c
	$(CC) -c $(CFLAGS) $< -o $@

compiling: $(OBJECTS)

clean:
	@$(RM) *.chk p *o gabedit *.com *.out *.log *.test test* *.cpp  *old *.ps .*swp *~ 

#make dependance
SRCS = $(OBJECTS:%.o=%.c)
dep :
	gcc -MM $(SRCS) > Dep.mk

-include Dep.mk
