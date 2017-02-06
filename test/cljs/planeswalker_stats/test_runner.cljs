(ns planeswalker-stats.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [planeswalker-stats.core-test]
   [planeswalker-stats.common-test]))

(enable-console-print!)

(doo-tests 'planeswalker-stats.core-test
           'planeswalker-stats.common-test)
