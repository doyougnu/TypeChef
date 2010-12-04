#!/bin/bash -vxe
#!/bin/bash -e
if [ -z "$jcppConfLoaded" ]; then
  source jcpp.conf
fi

javaOpts='$javaOpts -Xmx2G -Xms128m -Xss8M'

# For Java compiled stuff!
basePath=.

#mainClass="org.anarres.cpp.Main"
mainClass="de.fosd.typechef.typesystem.Main"

# Brute argument parsing
# The right thing to do would be to be a gcc replacement, parse its flags and
# select the ones we care about.
if [ $# -lt 1 ]; then
  echo "Not enough arguments!" >&2
  exit 1
fi
inp=$1
shift

. setupOutPaths.sh.inc

bash -c "time java -ea $javaOpts -cp \
$basePath/project/boot/scala-2.8.0/lib/scala-library.jar:\
$basePath/FeatureExprLib/lib/org.sat4j.core.jar:\
$basePath/PartialPreprocessor/lib/gnu.getopt.jar:\
$basePath/PartialPreprocessor/lib/junit.jar:\
$basePath/FeatureExprLib/target/scala_2.8.0/classes:\
$basePath/PartialPreprocessor/target/scala_2.8.0/classes:\
$basePath/ParserFramework/target/scala_2.8.0/classes:\
$basePath/CParser/target/scala_2.8.0/classes:\
$basePath/CTypeChecker/target/scala_2.8.0/classes \
  $mainClass \
  '$inp' 2> '$outErrT'|tee '$outDbgT'" \
  2> "$outTimeT" || true

cat "$outErrT" 1>&2
