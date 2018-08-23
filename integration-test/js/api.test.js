const api = require('./api')
const axios = require('axios')

const client = axios.create({
    baseURL: "http://localhost:8284",
    headers: {
        'Content-type': 'Application/json'},
    crossdomain: true})

//
// capture api
//
test('getCaptureIntByInt', () => api.getCaptureIntByInt(client, 66)
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(66)
    }))

test('getCaptureTextByText', () => api.getCaptureTextByText(client, "howdy!")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("howdy!")
    }))

test('getCaptureBoolByBool', () => api.getCaptureBoolByBool(client, true)
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(true)
    }))

test('getCaptureTransByTrans', () => api.getCaptureTextByText(client, "ToUpper")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("ToUpper")
    }))

//
// query param api
//
test('getQueryparamInt', () => api.getQueryparamInt(client, {value: 66})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(66)
    }))

test('getQueryparamText', () => api.getQueryparamText(client, {value: "howdy!"})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("howdy!")
    }))

test('getQueryparamBool', () => api.getQueryparamBool(client, {value: true})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(true)
    }))

test('getQueryparamTrans', () => api.getQueryparamTrans(client, {value: "ToUpper"})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("ToUpper")
    }))


//
// reqbody api
//
test('postReqbodyByInt', () => api.postReqbodyInt(client, 66)
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(66)
    }))

test('postReqbodyByText', () => api.postReqbodyText(client, "howdy!")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("howdy!")
    }))

test('postReqbodyByBool', () => api.postReqbodyBool(client, true)
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(true)
    }))

test('postReqbodyByTrans', () => api.postReqbodyTrans(client, "ToUpper")
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("ToUpper")
    }))

