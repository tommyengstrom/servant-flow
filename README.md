# servant-flow

This is a work in progress, not yet ready for use. Types are not yet added, everything is `any`.

## Generated client

Generating the client for

```haskell
data Transformation = ToUpper | ToLower
    deriving (Show, Generic)

type API = "changeCase"
        :> Capture "transformation" Transformation
        :> QueryParam "maxChars" Int
        :> QueryFlag "fromEnd"
        :> Get '[JSON] Text
    :<|> "user" :> ReqBody '[JSON] Text :> Post '[JSON] Text
```

Looks like this
```javascript
function getChangeCaseByTransformation(
    client /* : any */,
    transformation/* : any */,
    opts /* : {
        maxChars: any,
        fromEnd: any
        } */
    ){
    return client({
            url: [
                'changeCase',
                encodeURIComponent(transformation)
                ].join('/'),
            method: 'get',
            params: opts
            }
        )
    }
module.exports.getChangeCaseByTransformation = getChangeCaseByTransformation


function postUser(
    client /* : any */,
    data/* : any */
    ){
    return client({
            url: [
                'user'
                ].join('/'),
            method: 'post',
            data: data
            }
        )
    }
module.exports.postUser = postUser
```
## Run integration tests with
```
./run-tests
```

