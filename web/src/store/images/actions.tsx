import 'whatwg-fetch';
import { Dispatch }     from 'redux';
import { createAction } from 'redux-actions';


export const loadImages      = createAction('LOAD_IMAGES');
export const setImageScaling = createAction('SET_IMAGE_SCALING');

export const fetchImages = () => async (dispatch: Dispatch) => {
    const request = {
        headers:     { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:      'GET',
    };

    const response = await fetch('http://127.0.0.1:3001/api/image', request);
    const json     = await response.json();
    dispatch(loadImages(json.images));
};
