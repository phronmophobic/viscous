# Viscous

An visual data inspector/explorer that runs in constant space, time, and screen space.

![Overview](inspector.gif?raw=true)

![Resizing](inspector-resize.gif?raw=true)

## Dependency

```clojure
com.phronemophobic/viscous {:git/sha "599d80715206baf09dfdcd04f65aa8a02f6bb8a1"
                            :git/url "https://github.com/phronmophobic/viscous"}
```

## Usage

```clojure
(require '[com.phronemophobic.viscous :as viscous])


(def my-data {:a {:b 42}})

;; open inspector window
(viscous/inspect my-data)

```

## Status

Viscous is still pretty alpha. Behavior is subject to change. Feedback is welcome.

## Related

I'm generally interested in finding better ways to represent and explore medium sized heterogenous data. If you like viscous, you may also be interested in [treemap-clj](https://github.com/phronmophobic/treemap-clj).

## License

Copyright © 2021 Adrian Smith

Distributed under the Eclipse Public License version 1.0.
