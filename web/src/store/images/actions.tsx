import 'whatwg-fetch';
import { Dispatch }                      from 'redux';
import { createAction }                  from 'redux-actions';
import { fetchImages as apiFetchImages } from '../../Api';
import { image }                         from '../../types/image';


export const loadImages        = createAction('SET_IMAGES');
export const setGalleryScaling = createAction('SET_IMAGE_SCALING');
export const fetchImages       =
    () => async (dispatch: Dispatch) => {
        const json = await apiFetchImages(null) as unknown as { images: image[] };
        dispatch(loadImages(json.images));
    };
