#!/bin/bash

idris --build react.ipkg &&
idris --mkdoc react.ipkg &&
idris --build example.ipkg
