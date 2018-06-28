function createClient(token/* : string */, baseURL/* : string */)
    {
        return axios.create({headers: {Authorization: token}, baseURL: baseURL});
    };



function getChangeCaseByTransformation(
    client/* : any */,
    transformation/* : any */,
    params /* : { maxChars?: any, fromEnd?: any } */
    )/* : any */
    {
        let url = [ "changeCase"
                  , encodeURIComponent(transformation)
                  ].join('/')
        return client({
            url: url,
            method: 'get',
            params: params
            })
    };

function postUser(
    client/* : any */,
    data /* : any */
    )/* : any */
    {
        let url = [ "user"
                  ].join('/')
        return client({
            url: url,
            method: 'post',
            data: data
            })
    };

module.exports = {
    postUser: postUser,
    getChangeCaseByTransformation: getChangeCaseByTransformation
    }

