# servant-flow

[![Build Status](https://travis-ci.org/henrylabs/servant-flow.svg?branch=master)](https://travis-ci.org/henrylabs/servant-flow)

Generate Axios clients for Servant APIs with [Flow](https://flow.org) types.

We use a `Generic`-derivable type class that takes the same `Options` argument as used in the `aeson` package, to ensure correspondence with `FromJSON`/`ToJSON` instances. The generic instances support records and simple sum types, while arbitrary sums of products are not yet supported unless you provide the instances explicitly.

NOTE: As this is a young package, take reasonable precautions before fully trusting it in production.

## Example

This Servant API

```haskell
data Transformation = ToUpper | ToLower
    deriving (Show, Generic, ToJSON, FromJSON, Flow)

type API = "changeCase"
        :> Capture "transformation" Transformation
        :> QueryParam "maxChars" Int
        :> QueryFlag "fromEnd"
        :> Get '[JSON] Text
    :<|> "user" :> ReqBody '[JSON] Text :> Post '[JSON] Text
```

leads to the following client code via `generateFlowClient (Proxy @API) defaultCodeGenOptions`:

```javascript
// @flow

const axios = require('axios')

function createClient(
    token/* : string */,
    baseURL/* : string */
    ){
    return axios.create({
            headers: {
                Authorization: token
                },
            baseURL: baseURL
            }
        )
    }
module.exports.createClient = createClient

/*::

type Transformation = "ToUpper" | "ToLower"

*/


function getChangeCaseByTransformation(
    client /* : any */,
    transformation /* : Transformation */,
    opts /* : {
        maxChars : ?(number),
        fromEnd : boolean
        } */
    )/* : Promise<{ status: string, data: string }> */{
    return client({
            url: [
                'changeCase',
                encodeURIComponent(transformation.toString())
                ].join('/'),
            method: 'get',
            params: opts
            }
        )
    }
module.exports.getChangeCaseByTransformation = getChangeCaseByTransformation


function postUser(
    client /* : any */,
    data /* : string */
    )/* : Promise<{ status: string, data: string }> */{
    return client({
            url: [
                'user'
                ].join('/'),
            method: 'post',
            data: JSON.stringify(data)
            }
        )
    }
module.exports.postUser = postUser
```

See either of the two test suites for details.

## Run integration tests with
```
./run-tests
```
