## Performance tests

```
::Benchmark pl.softech.collection.RadixTree.get::
cores: 4
hostname: bpol0845
name: Java HotSpot(TM) 64-Bit Server VM
osArch: amd64
osName: Linux
vendor: Oracle Corporation
version: 25.144-b01
Parameters(pre -> , post -> ): 10.237927
Parameters(pre -> , post -> x): 6.075446
Parameters(pre -> x, post -> ): 5.33231
Parameters(pre -> x, post -> x): 5.412436

::Benchmark pl.softech.collection.RadixTree.startsWith::
cores: 4
hostname: bpol0845
name: Java HotSpot(TM) 64-Bit Server VM
osArch: amd64
osName: Linux
vendor: Oracle Corporation
version: 25.144-b01
Parameters(pre -> , post -> ): 5.150289
Parameters(pre -> , post -> x): 5.846253
Parameters(pre -> x, post -> ): 5.277848
Parameters(pre -> x, post -> x): 5.23772

::Benchmark scala.collection.immutable.Set.get::
cores: 4
hostname: bpol0845
name: Java HotSpot(TM) 64-Bit Server VM
osArch: amd64
osName: Linux
vendor: Oracle Corporation
version: 25.144-b01
Parameters(pre -> , post -> ): 89.861085
Parameters(pre -> , post -> x): 89.999667
Parameters(pre -> x, post -> ): 87.032065
Parameters(pre -> x, post -> x): 92.255482

::Benchmark scala.collection.immutable.Set.startsWith::
cores: 4
hostname: bpol0845
name: Java HotSpot(TM) 64-Bit Server VM
osArch: amd64
osName: Linux
vendor: Oracle Corporation
version: 25.144-b01
Parameters(pre -> , post -> ): 19.704876
Parameters(pre -> , post -> x): 77.428846
Parameters(pre -> x, post -> ): 75.040239
Parameters(pre -> x, post -> x): 79.914986
```