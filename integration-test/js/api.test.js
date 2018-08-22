const api = require('./api')
const axios = require('axios')

const client = axios.create({
    baseURL: "http://localhost:8284",
    headers: {
        'Content-type': 'Application/json'},
    crossdomain: true})


test('getCaptureByInt', () => api.getCaptureByInt(client, 66)
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(66)
    }))

test('getCaptureByText', () => api.getCaptureByInt(client, "howdy!")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("howdy!")
    }))

test('getCaptureByTransformation', () => api.getCaptureByInt(client, "ToUpper")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("ToUpper")
    }))

