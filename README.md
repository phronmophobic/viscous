# Viscous

An visual data inspector/explorer that runs in constant space, time, and screen space.

![Overview](inspector.gif?raw=true)

![Resizing](inspecotr-resize.gif?raw=true)

## Dependency

```clojure
com.phronemophobic/membrane {:git/sha "9a9c91e69629bade7cbe784bc7eb60241cf404b2"}
```

## Usage

```clojure
(require '[com.phronemophobic.viscous :as viscous])


(def my-data {:a {:b 42}})

;; open inspector window
(viscous/inspect my-data)

```

## License

Copyright © 2021 Adrian Smith

Distributed under the Eclipse Public License version 1.0.
