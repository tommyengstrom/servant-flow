//@flow

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

// 'QueryParams' parameters can be specified more than once
test('getQueryparamTexts', () => api.getQueryparamTexts(client, {value: ["FOO", "BAR"]})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe("FOOBAR")
    }))

test('getQueryparamBool - flag set', () => api.getQueryparamBool(client, {true: true})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(true)
    }))

// Query flags may be omitted
test('getQueryparamBool - flag unset', () => api.getQueryparamBool(client, {true: null})
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toBe(false)
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

let bar = { barFoo: 8, barBool: true, barTransformation: "ToUpper" }


test('postReqbodyByBar', () => api.postReqbodyBar(client, bar)
    .then(response => {
        expect(response.status).toBe(200)
        expect(response.data).toEqual(bar)
    }))
