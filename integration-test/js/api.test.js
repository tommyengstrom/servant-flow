const api = require('./api')
const axios = require('axios')

const client = axios.create({
    baseURL: "http://localhost:8080",
    headers: {
        'Content-type': 'Application/json'},
    crossdomain: true})


test('postUser', () => api.postUser(client, '"olle"')
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("like a glove!")
    }))

test('getChangeCaseByTransformation', () => api.getChangeCaseByTransformation(client, 'ToUpper', {maxChars: 4, fromEnd: true})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("oh yeah!")
    }))

