/// other ==> main

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftppc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/fake-build-whole-module.py" -output-file-map %t/output.json -whole-module-optimization ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s
// RUN: %FileCheck -check-prefix=CHECK-RECORD %s < %t/main~buildrecord.swiftdeps

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Produced main.o

// CHECK-RECORD-DAG: "./main.swift": [
// CHECK-RECORD-DAG: "./other.swift": [


// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && not %swiftppc_driver -c -driver-use-frontend-path "%{python};%S/../Inputs/fail.py" -output-file-map %t/output.json -whole-module-optimization ./main.swift ./other.swift -module-name main -j1 -v 2>&1

// Just don't crash.
