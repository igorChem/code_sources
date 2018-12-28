#ifndef __GABEDIT_TYPESMG_H__
#define __GABEDIT_TYPESMG_H__
#define MAXBOUNDARY  12
typedef enum
{
	GABEDIT_INTERIOR = 0,
	GABEDIT_BOUNDARY = 1,
	GABEDIT_ALL = 2
} OperationTypeMG;
typedef enum
{
	GABEDIT_LAPLACIAN_2 = 2,
	GABEDIT_LAPLACIAN_4 = 4,
	GABEDIT_LAPLACIAN_6 = 6,
	GABEDIT_LAPLACIAN_8 = 8,
	GABEDIT_LAPLACIAN_10 = 10,
	GABEDIT_LAPLACIAN_12 = 12,
	GABEDIT_LAPLACIAN_14 = 14,
	GABEDIT_LAPLACIAN_16 = 16,
	GABEDIT_LAPLACIAN_18 = 18,
	GABEDIT_LAPLACIAN_20 = 20,
	GABEDIT_LAPLACIAN_22 = 22,
	GABEDIT_LAPLACIAN_24 = 24
} LaplacianOrderMG;
typedef enum
{
	GABEDIT_CONDITION_PERIODIC = 0,
	GABEDIT_CONDITION_CLUSTER  = 1,
	GABEDIT_CONDITION_MULTIPOL = 2,
	GABEDIT_CONDITION_EWALD = 3,
	GABEDIT_CONDITION_EXTERNAL = 4
} Condition;
typedef enum
{
	GABEDIT_UNK = 0,
	GABEDIT_CG = 1,
	GABEDIT_MG = 2,
	GABEDIT_EXACT = 3,
} PoissonSolverMethod;
#endif /* __GABEDIT_TYPESMG_H__*/