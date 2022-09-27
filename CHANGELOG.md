# Changelog pre v1

## 0.6.0 [2022-09-27]

### Enhancements

* Added documentation to the main module

## 0.5.0 (2022-09-02)

### Enhancements

* The behaviour have been cleaned up and the relationship between the behaviour and the API have been cleaned up. In particular, the run_of_zeroes is now done in the backend.
* The array backend have been dropped. If you want it back, PR welcome.

## 0.4.0 (2022-06-25)

### Enhancements

* The behaviour have been cleaned up and the relationship between the behaviour and the API have been cleaned up.
* There is now a version atom in each HLL generated. This is not too important right now, but will be used in the future to block merging two HLL with incompatible versions (as an example if the hashing strategy changed)
