set(CTEST_PROJECT_NAME "Avogadro")
set(CTEST_NIGHTLY_START_TIME "21:00:00 EST")

set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "my.cdash.org")
set(CTEST_DROP_LOCATION "/submit.php?project=Avogadro")
set(CTEST_DROP_SITE_CDASH TRUE)

set(CTEST_PROJECT_SUBPROJECTS
  avogadro
  engines
  tools
  extensions
  colors
  avogadro-app)
