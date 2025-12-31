## Table Pages Design 

### The Pairs Tables 

The pairs tables are currently handled by ::Controller::Pairs package which registers the routes

* /Generals/:uiTarget/:buffActivation/pair-comparison
* /Generals/:uiTarget/:buffActivation/pair/data.json
* /:uiTarget/:buffActivation/pair-details-stream

### The Single General Tables

The single general tables are currently handled by the ::Controller::Generals package which registers the routes

* /Generals/:uiTarget/:buffActivation/comparison
* /Generals/:uiTarget/:buffActivation/data.json
* /Generals/:uiTarget/:buffActivation/:isPrimary/details-stream

### The Client 

Both use typescript clients that are compiled from lib/Generals/**/*.ts. Some of the files are shared in common, particularly the zod definitions in lib/Generals/GeneralRowSchemas.ts but most are in page specific sub folders. 

Both use tanstack Store for state storage, and tanstack Table as the table controller.  Both use Lit elements, and Spectrum-CSS for styling.  Some of the styling is embedded in the elements, and some is compiled from the style documents in share/styles/**/*.css 

### Desired State

* Update the beginning of this document (the part above the Desired State heading) with any changes to routes or client library locations made during this refactoring. 
* Continue to use Zod definitions on the client side to ensure that data shape is preserved and nothing nefarious is going on. 
* Continue to use Spectrum-CSS for styling. 
* Continue to use Lit elements compiled from Typescript. 
* Continue to keep the client side code light and minimal. 
* Continue to use tanstack Store for client state management.
* Continue to use tanstack Table for table management. 
* The Client is responsible for the presentation layer only.  All computations should happen server side. 
* Use PDL as per the current pair table implementation to obtain massive performance increases.  Utilize this PDL based
  solution in *both* tables. 
* When deployed to EC2, we will be running on hypnotoad with a number of hypnotoad workers that do not share memory   
  state except via the persistence layer. We cannot know which worker a given call will hit, even within the same overall
  session between the client and the server. *Each connection to a specific route must be idempotent.*
* The initial call to the User visible route should trigger the download of the typescript client. 
* the user visible route should contain sufficient information either in the route itself or in the url's query string
  such that the user can bookmark it, or copy it and send it to a friend, who can then replicate the same view. 
  - buff activation type
  - troop type
  - levels for each of the various filters
    - primary ascending level
    - primary covenant level
    - level for each (independent) specialty (there are 4 of them).
      - constraint: the 4th specialty must be at value 'none' unless the first three are at level 'gold'.
      - constraint: the 4th specialty must *not* be at value 'none' if the first three are at level 'gold'.
  - Filter values must be validated by both client and server. 
    - client side validation via zod schema
    - server side validation against the constant values in the ::Role::Constants:: namespace 
    - checking on both sides implements a layered approach to security. 
  - The User should be able to filter on what primary generals are included for the pairs table, or included at all for 
    the single general tables (it only includes primaries).  
    - Generals can be validated for existence on the server side
    - Generals cannot be validated client side except for conformance to the overall 'shape' of the data via zod schema.
    - the existing filter mechanism works from a technical standpoint but is a poor UX, and needs revisited to behave more in line with a traditional multi-select combo-box. 
* Changing the filter values should refresh the data in the tables. 
  * Removing one or more Generals from the selected list should not require a call to the server, that is just a UX update.
  * All other changes should be computed on the server side. The UI should reflect that it is pending an update. 
  * Favor progressive updates of the page, when partial data is available 
  * Favor an AJAX style approach to page updates rather than posting the entire page.
* Use Server Sent Events to assist with implementing an AJAX style, progressively updated experience. 
* For asynchronous work, perl Minion is available server side. 
  * when deployed to EC2, you can run at most 4 jobs at a time. 
  * each new Minion job requires that Mojolicious spawn a new worker thread when the job transitions from 'inactive' to 'active.  Threads are retired when the job is complete (either success or fail).  
  * Minion jobs should extend the Game::EvonyTKR::External::JobBase package and implement a 'task_name' method. 
  * Favor approaches that balance the cost of forking new threads with the benefits of more granular updates and the ability to send more frequent, and consistently timed, SSE.  Avoid both excessive batch size and excessive granularity. 
* When interacting with persistence, use the ::Role::Persistence:: packages.  Pretend that Perl allows you to mark things as being private/protected, and that the actual persistence instance is scoped to the Role, and not visible outside it. 
* Single Generals should be evaluated as if all generals are primary generals.  I have some minimal support for tables of secondaries, but as secondaries are in practice only interesting in relation to a particular primary, this has not proven popular or needed. 
* Single Generals get 3 generic skill books. 
* Pairs have a Primary and a Secondary general in each pair.  
* Only Primary generals get ascending attributes.  Secondary generals do not. 
* Pairs get 6 generic skill books. 
  * The skill books must be compatible with *both* the Primary and Secondary. 
  * A General may experience a 'Partial Conflict' with a skill book.  Two identical 'Partial Conflicts' is equivalent to 
    a full conflict and disqualifies a skill book.   Single 'Partial Conflicts' can be ignored. 
* Generic skill books are variable but deterministic based on buff activation type and troop type.  If you know both of these, *and the conflict information* you know which generic books will be chosen. There is a helper in Game::EvonyTKR::Role::Books to assist with this deterministic choice. 
* A General's built in book may be *missing* if not populated via call to persistence (a helper method is available in the Model), but is always *active* in any computations. 
* Thus a table with all filters set to 'none' (and at least one general chosen) will show the results of that general (or pairs with that general as primary for the pairs tables) computed with the built in book and any relevant generic books. 
* Any persistence keys should be stored as all lower case, and normalized using the shared method from Game::EvonyTKR::Role::Common.  Do not implement multiple duplicative normalization methods. 

### Current bottlenecks to Avoid

* Each time you need to iterate a list of generals and/or pairs and convert to or from the 'wire form' to fully inflated objects scoped within the Game::EvonyTKR::Model:: namespace, there is a time cost.  Each represents a deeply nested interaction of multiple such objects, that as a totality react poorly to serialization (hence the 'wire form'), but when inflated offer a rich API to get the information we need to build the user visible relationships between data elements and results from derived/computed data.  Currently we inflate too frequently.  Avoid both the current problem (over inflation), but also the inverse, inflating too rarely.
* Calling the various populate methods within a General increases object complexity and thus time to inflate. All populate methods are optional, but called by default when inflating. The factory only partially implements disabling populate methods.  Consider the use case for the inflated object and if you need the object fully inflated, partially inflated, or barely present to achieve the workflow goals. The less inflated the faster we go, but the less we can compute.
* Poor state management on the client side.  Right now the client is experiencing an eventual consistency jitter that results in 2-3 calls to the data APIs per filter state. This creates unnecessary server load, despite debouncing attempts. 
* Inconsistent optimization. Right now the two sets of tables each reflect hybrid states where each has been only partially and incompletely kept in sync with improvements made to the other.  While there are some inherent differences between pairs and single generals, we should reuse as much as possible and keep the two approaches as aligned as possible.  Avoiding this may mean creating a new Role that both the ::Controller::Generals and ::Controller::Pairs compose in to provide common methods that both use for common aspects of table page workflows.


### Other considerations

* Do not use Mojo::JSON.  If JSON encoding/decoding are necessary, use Game::EvonyTKR::Role::JSON
* Even in Wire form, objects in SSE events must be Base64 encoded to preserve UTF8 successfully. 
* It may well be optimal to change the zod schemas in the client.  That's fine, as long as we continue to validate inputs from the server as untrusted by the client, and inputs from the client to the server as equally untrusted (since they traverse the public internet). It is NOT acceptable to change the constants in Role::Constants:: -- these are based on values from the game. It *is* acceptable to add helpers to the Roles to make using the existing constants (validating against them, normalizing to them, and/or transforming them in deterministic ways -- ie to make a more UX friendly version ) easier. 
* UTF-8 data should be assumed everywhere.  It *already* happens in many places already, and those places will continue to proliferate as generals get added to the application. 
* Assume a Perl 5.42 baseline.
* Assume a Typescript 24.12.0 baseline.  Assume typescript will be transpiled to Javascript as part of deployment.
* Do cooperate with the central debug logging management for typescript in lib/localDebug.ts
* Do make use of the central perl logging infrastructure in Game::EvonyTKR::Role::Logging
