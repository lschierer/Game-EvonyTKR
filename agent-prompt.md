* This project uses two primary programing languages: perl (version 5.42 or higher) and Typescript.  There are more details in the Readme.md file.
* The text content is written in Markdown, with a number of GFM features including footnotes, asides, and tables.  Pandoc was chosen for parsing in part for OSX compatibility, in part for feature compatibility.
* The project should largely self configure.  Where possible, it should be modular, with the modules being discovered and loaded as part of startup.
  * Where this is not possible, for example because there is an order in which things need to be loaded, a separate namespace for the auto loading and statically configured modules should be used.
* It will be deployed to AWS
  * When deploying to AWS, there will be a pre-production and production stack.
  * This should be scaled for about 500 users, 5-10 of which might be conccurrent.  Each user will make between 1 and 5 route requests at a time.
  * Most users are from English speaking countries, with 90% from the US
  * while availability is nice, economy is a requirement

* This project is developing an information resource for players of the game Evony The Kings Return (EvonyTKR).
* Some of the information is statically loaded from YAML files.
* Some of the information is computed/derived from previously loaded information.
* Testing has shown that staying within both budget and performance requirements is the greatest difficulty with this overall project. This represents the 5th generation of the project attempting to improve performance, decrease cost, or both.
* The current architecture attempt is Perl Mojolicous with asynchronous work being done by perl Minion workers.  the Minion workers are co-located with the Mojolicious frontend.
* Some of the Mojolicious routes serve some *very minimal* Typescript for things that cannot be done on the server while staying within performance and budget.
* Testing has shown that the Typescript content must be kept minimal or the performance becomes unacceptably poor due to the computed data types.
* Testing has shown that precomputing all data types is prohibitive due in part to the update cycle on the data and in part due to the combinatorial nature of the potential requests as related to the static data.
* We do have a sqlite database, because Minion requires the presence of *some* database.  We use it for custom purposes as little as possible, and for *no* long term state.
* We use Memcached for IPC, and for caching both static and computed data types.
* We prefer the perl Test2::V0 test harness for testing the perl packages.

* You should be an expert in AWS infrastructure deployment using CDK, perl programing, and Typescript programing.
