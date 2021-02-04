/** @file omd2omdCmd.hpp
 *  @brief The header file for the command line option parser
 *  generated by GNU Gengetopt version 2.23
 *  http://www.gnu.org/software/gengetopt.
 *  DO NOT modify this file, since it can be overwritten
 *  @author GNU Gengetopt */

#ifndef OMD2OMDCMD_H
#define OMD2OMDCMD_H

/* If we use autoconf.  */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h> /* for FILE */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#ifndef CMDLINE_PARSER_PACKAGE
/** @brief the program name (used for printing errors) */
#define CMDLINE_PARSER_PACKAGE "omd2omd"
#endif

#ifndef CMDLINE_PARSER_PACKAGE_NAME
/** @brief the complete program name (used for help and version) */
#define CMDLINE_PARSER_PACKAGE_NAME "omd2omd"
#endif

#ifndef CMDLINE_PARSER_VERSION
/** @brief the program version */
#define CMDLINE_PARSER_VERSION ""
#endif

/** @brief Where the command line options are stored */
struct gengetopt_args_info
{
  const char *help_help; /**< @brief Print help and exit help description.  */
  const char *version_help; /**< @brief Print version and exit help description.  */
  char * input_arg;	/**< @brief use specified input (.omd, .dump, .eor) file.  */
  char * input_orig;	/**< @brief use specified input (.omd, .dump, .eor) file original value given at command line.  */
  const char *input_help; /**< @brief use specified input (.omd, .dump, .eor) file help description.  */
  char * output_arg;	/**< @brief use specified output file.  */
  char * output_orig;	/**< @brief use specified output file original value given at command line.  */
  const char *output_help; /**< @brief use specified output file help description.  */
  int repeatX_arg;	/**< @brief make the system repeat in the x direction (default='1').  */
  char * repeatX_orig;	/**< @brief make the system repeat in the x direction original value given at command line.  */
  const char *repeatX_help; /**< @brief make the system repeat in the x direction help description.  */
  int repeatY_arg;	/**< @brief make the system repeat in the y direction (default='1').  */
  char * repeatY_orig;	/**< @brief make the system repeat in the y direction original value given at command line.  */
  const char *repeatY_help; /**< @brief make the system repeat in the y direction help description.  */
  int repeatZ_arg;	/**< @brief make the system repeat in the z direction (default='1').  */
  char * repeatZ_orig;	/**< @brief make the system repeat in the z direction original value given at command line.  */
  const char *repeatZ_help; /**< @brief make the system repeat in the z direction help description.  */
  double translateX_arg;	/**< @brief translate all x coordinates by some amount (default='0.0').  */
  char * translateX_orig;	/**< @brief translate all x coordinates by some amount original value given at command line.  */
  const char *translateX_help; /**< @brief translate all x coordinates by some amount help description.  */
  double translateY_arg;	/**< @brief translate all y coordinates by some amount (default='0.0').  */
  char * translateY_orig;	/**< @brief translate all y coordinates by some amount original value given at command line.  */
  const char *translateY_help; /**< @brief translate all y coordinates by some amount help description.  */
  double translateZ_arg;	/**< @brief translate all z coordinates by some amount (default='0.0').  */
  char * translateZ_orig;	/**< @brief translate all z coordinates by some amount original value given at command line.  */
  const char *translateZ_help; /**< @brief translate all z coordinates by some amount help description.  */
  double rotatePhi_arg;	/**< @brief rotate all coordinates Euler angle Phi (default='0.0').  */
  char * rotatePhi_orig;	/**< @brief rotate all coordinates Euler angle Phi original value given at command line.  */
  const char *rotatePhi_help; /**< @brief rotate all coordinates Euler angle Phi help description.  */
  double rotateTheta_arg;	/**< @brief rotate all coordinates Euler angle Theta (default='0.0').  */
  char * rotateTheta_orig;	/**< @brief rotate all coordinates Euler angle Theta original value given at command line.  */
  const char *rotateTheta_help; /**< @brief rotate all coordinates Euler angle Theta help description.  */
  double rotatePsi_arg;	/**< @brief rotate all coordinates Euler angle Psi (default='0.0').  */
  char * rotatePsi_orig;	/**< @brief rotate all coordinates Euler angle Psi original value given at command line.  */
  const char *rotatePsi_help; /**< @brief rotate all coordinates Euler angle Psi help description.  */
  int repairMolecules_arg;	/**< @brief rewrap molecules around the molecular center of mass (default='1').  */
  char * repairMolecules_orig;	/**< @brief rewrap molecules around the molecular center of mass original value given at command line.  */
  const char *repairMolecules_help; /**< @brief rewrap molecules around the molecular center of mass help description.  */
  
  unsigned int help_given ;	/**< @brief Whether help was given.  */
  unsigned int version_given ;	/**< @brief Whether version was given.  */
  unsigned int input_given ;	/**< @brief Whether input was given.  */
  unsigned int output_given ;	/**< @brief Whether output was given.  */
  unsigned int repeatX_given ;	/**< @brief Whether repeatX was given.  */
  unsigned int repeatY_given ;	/**< @brief Whether repeatY was given.  */
  unsigned int repeatZ_given ;	/**< @brief Whether repeatZ was given.  */
  unsigned int translateX_given ;	/**< @brief Whether translateX was given.  */
  unsigned int translateY_given ;	/**< @brief Whether translateY was given.  */
  unsigned int translateZ_given ;	/**< @brief Whether translateZ was given.  */
  unsigned int rotatePhi_given ;	/**< @brief Whether rotatePhi was given.  */
  unsigned int rotateTheta_given ;	/**< @brief Whether rotateTheta was given.  */
  unsigned int rotatePsi_given ;	/**< @brief Whether rotatePsi was given.  */
  unsigned int repairMolecules_given ;	/**< @brief Whether repairMolecules was given.  */

  char **inputs ; /**< @brief unnamed options (options without names) */
  unsigned inputs_num ; /**< @brief unnamed options number */
} ;

/** @brief The additional parameters to pass to parser functions */
struct cmdline_parser_params
{
  int override; /**< @brief whether to override possibly already present options (default 0) */
  int initialize; /**< @brief whether to initialize the option structure gengetopt_args_info (default 1) */
  int check_required; /**< @brief whether to check that all required options were provided (default 1) */
  int check_ambiguity; /**< @brief whether to check for options already specified in the option structure gengetopt_args_info (default 0) */
  int print_errors; /**< @brief whether getopt_long should print an error message for a bad option (default 1) */
} ;

/** @brief the purpose string of the program */
extern const char *gengetopt_args_info_purpose;
/** @brief the usage string of the program */
extern const char *gengetopt_args_info_usage;
/** @brief the description string of the program */
extern const char *gengetopt_args_info_description;
/** @brief all the lines making the help output */
extern const char *gengetopt_args_info_help[];

/**
 * The command line parser
 * @param argc the number of command line options
 * @param argv the command line options
 * @param args_info the structure where option information will be stored
 * @return 0 if everything went fine, NON 0 if an error took place
 */
int cmdline_parser (int argc, char **argv,
  struct gengetopt_args_info *args_info);

/**
 * The command line parser (version with additional parameters - deprecated)
 * @param argc the number of command line options
 * @param argv the command line options
 * @param args_info the structure where option information will be stored
 * @param override whether to override possibly already present options
 * @param initialize whether to initialize the option structure my_args_info
 * @param check_required whether to check that all required options were provided
 * @return 0 if everything went fine, NON 0 if an error took place
 * @deprecated use cmdline_parser_ext() instead
 */
int cmdline_parser2 (int argc, char **argv,
  struct gengetopt_args_info *args_info,
  int override, int initialize, int check_required);

/**
 * The command line parser (version with additional parameters)
 * @param argc the number of command line options
 * @param argv the command line options
 * @param args_info the structure where option information will be stored
 * @param params additional parameters for the parser
 * @return 0 if everything went fine, NON 0 if an error took place
 */
int cmdline_parser_ext (int argc, char **argv,
  struct gengetopt_args_info *args_info,
  struct cmdline_parser_params *params);

/**
 * Save the contents of the option struct into an already open FILE stream.
 * @param outfile the stream where to dump options
 * @param args_info the option struct to dump
 * @return 0 if everything went fine, NON 0 if an error took place
 */
int cmdline_parser_dump(FILE *outfile,
  struct gengetopt_args_info *args_info);

/**
 * Save the contents of the option struct into a (text) file.
 * This file can be read by the config file parser (if generated by gengetopt)
 * @param filename the file where to save
 * @param args_info the option struct to save
 * @return 0 if everything went fine, NON 0 if an error took place
 */
int cmdline_parser_file_save(const char *filename,
  struct gengetopt_args_info *args_info);

/**
 * Print the help
 */
void cmdline_parser_print_help(void);
/**
 * Print the version
 */
void cmdline_parser_print_version(void);

/**
 * Initializes all the fields a cmdline_parser_params structure 
 * to their default values
 * @param params the structure to initialize
 */
void cmdline_parser_params_init(struct cmdline_parser_params *params);

/**
 * Allocates dynamically a cmdline_parser_params structure and initializes
 * all its fields to their default values
 * @return the created and initialized cmdline_parser_params structure
 */
struct cmdline_parser_params *cmdline_parser_params_create(void);

/**
 * Initializes the passed gengetopt_args_info structure's fields
 * (also set default values for options that have a default)
 * @param args_info the structure to initialize
 */
void cmdline_parser_init (struct gengetopt_args_info *args_info);
/**
 * Deallocates the string fields of the gengetopt_args_info structure
 * (but does not deallocate the structure itself)
 * @param args_info the structure to deallocate
 */
void cmdline_parser_free (struct gengetopt_args_info *args_info);

/**
 * Checks that all the required options were specified
 * @param args_info the structure to check
 * @param prog_name the name of the program that will be used to print
 *   possible errors
 * @return
 */
int cmdline_parser_required (struct gengetopt_args_info *args_info,
  const char *prog_name);


#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* OMD2OMDCMD_H */