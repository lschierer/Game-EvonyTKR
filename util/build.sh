#!/bin/sh -x 



CPANM=`which cpanm`;
echo ${CPANM};

${CPANM} Software::License::CC_BY_4_0 || exit 2

${CPANM} --installdeps --notest Dist::Zilla || exit 1
${CPANM} --installdeps --notest Dist::Zilla::App::Command::authordeps || exit 1
${CPANM} --installdeps --notest Dist::Zilla::App::Command::listdeps || exit 1
${CPANM} --notest Dist::Zilla  || exit 1
${CPANM} --notest Dist::Zilla::App::Command::authordeps || exit 1
${CPANM} --notest Dist::Zilla::App::Command::listdeps || exit 1

DZIL=`which dzil`;

for i in `grep authordep dist.ini  | cut -d ' ' -f 3`; do 
    echo "${i}"
    ${CPANM} --installdeps --notest --with-recommends ${i} || exit 2
    ${CPANM} --notest ${i} || exit 3
    echo;
done

echo '---- author deps complete ----';

${DZIL} listdeps --missing | sort |grep -v ^builtin | ${CPANM} --installdeps --notest --with-feature=accelerate 

echo '---- standard deps complete ----';

${DZIL} build

echo '---- build complete ----';
