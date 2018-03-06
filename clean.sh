#!/bin/bash

idris --clean react.ipkg
idris --clean example.ipkg
rm -rf react_doc
