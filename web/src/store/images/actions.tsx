import 'whatwg-fetch';
import { Dispatch }     from 'redux';
import { createAction } from 'redux-actions';
import { getImages }    from '../../api/images';


export const loadImages        = createAction('LOAD_IMAGES');
export const setGalleryScaling = createAction('SET_IMAGE_SCALING');

export const fetchImages = () => async (dispatch: Dispatch) => {
    const json = await getImages();
    dispatch(loadImages(json.images));
};
