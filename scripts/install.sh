#!/bin/bash
# Script that installs files and performs basic translation of text files.
# Citrix Systems Inc, 2011
#
# Usage: ./install.sh <mode> <file1> .. <filen> <dest>
#
# Replaces the following strings:
#
# @OPTDIR@ -> ${OPTDIR}
# @ETCXENDIR@ -> ${ETCXENDIR}
# @VARPATCHDIR@
# @ETCXENDIR@
# @OPTDIR@
# @PLUGINDIR@
# @HOOKSDIR@
# @INVENTORY@
# @XAPICONF@
# @LIBEXECDIR@
# @SCRIPTSDIR@

#set -x

MODE=${1}
NUM_FILES=$(($#-2))
FILES=${@:2:$NUM_FILES}
DEST=${!#}

INSTALL="install -m ${MODE}"

for FILE in ${FILES}; do
  ${INSTALL} ${FILE} ${DEST} || exit 1
  if [ -d ${DEST} ]; then
    BASENAME=`basename ${FILE}`
    NEWFILE="${DEST}/${BASENAME}"
    NEWFILE=${NEWFILE//\/\//\/}
  else
    NEWFILE=${DEST}
  fi
  if file ${NEWFILE} | grep -q "text"; then
    sed -i -e "s!@OPTDIR@!${OPTDIR}!g" \
        -e "s!@BINDIR@!${BINDIR}!g" \
        -e "s!@ETCDIR@!${ETCDIR}!g" \
        -e "s!@ETCXENDIR@!${ETCXENDIR}!g" \
        -e "s!@VARPATCHDIR@!${VARPATCHDIR}!g" \
        -e "s!@PLUGINDIR@!${PLUGINDIR}!g" \
        -e "s!@HOOKSDIR@!${HOOKSDIR}!g" \
        -e "s!@INVENTORY@!${INVENTORY}!g" \
        -e "s!@XAPICONF@!${XAPICONF}!g" \
        -e "s!@LIBEXECDIR@!${LIBEXECDIR}!g" \
        -e "s!@SCRIPTSDIR@!${SCRIPTSDIR}!g" \
        -e "s!@SHAREDIR@!${SHAREDIR}!g" \
        -e "s!@WEBDIR@!${WEBDIR}!g" \
        -e "s!@CLUSTER_STACK_ROOT@!${CLUSTER_STACK_ROOT}!g" \
         ${NEWFILE}
  fi
done
