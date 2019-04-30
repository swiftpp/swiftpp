// RUN: not %target-ppswiftc_driver -color-diagnostics -emit-executable -o %t %s 2>&1 | %FileCheck %s

// CHECK: [0m1 = 2{{$}}
1 = 2
