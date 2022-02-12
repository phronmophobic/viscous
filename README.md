# Viscous

A visual data inspector/explorer that runs in constant space, time, and screen space.

### Overview

![Overview](inspector.gif?raw=true)

### Resizing
![Resizing](inspector-resize.gif?raw=true)

### Paging
![Paging](paging.gif?raw=true)

### Navigating
![Navigating](navigation.gif?raw=true)

## Web Demo

Try it! https://phronmophobic.github.io/viscous/

## Dependency

#### deps.edn
```clojure
com.phronemophobic/viscous {:mvn/version "1.1"}
```

#### lein
```clojure
[com.phronemophobic/viscous "1.1"]
```

## Usage

```clojure
(require '[com.phronemophobic.viscous :as viscous])

(def my-data {:a {:b 42}})

;; open inspector window
(viscous/inspect my-data)

```

## Status

Viscous is in beta. Behavior is subject to change. Feedback is welcome.

## Related

I'm generally interested in finding better ways to represent and explore medium sized heterogenous data. If you like viscous, you may also be interested in [treemap-clj](https://github.com/phronmophobic/treemap-clj).

## License

Copyright © 2021 Adrian Smith

Distributed under the Eclipse Public License version 1.0.
