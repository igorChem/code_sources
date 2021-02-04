This is a recipe for building the current development package into a conda
binary.

The installation on travis-ci is done by building the conda package, installing
it, running the tests, and then if successful pushing the package to binstar
(and the docs to AWS S3). The binstar auth token is an encrypted environment
variable generated using:

anaconda auth -n mdtraj-travis -o omnia --max-age 22896000 -c --scopes api:write

and then saved in the environment variable ANACONDA_TOKEN.

You can set up travis to store an encrypted token via

gem install travis
travis encrypt ANACONDA_TOKEN=xx

where xx is the token output by binstar.  The final command should print a line (containing 'secure') for inclusion in your .travis.yml file.
