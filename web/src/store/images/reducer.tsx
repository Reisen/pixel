import { State }         from './types';
import { handleActions } from 'redux-actions';
import { lensProp, set } from 'ramda';


const initialState: State = {
    galleryScalingMode: 'cover',
    images:             [],
};

export const imageReducer = handleActions<State>({
    'SET_IMAGES':        (state, action) => set(lensProp('images'), action.payload)(state),
    'SET_IMAGE_SCALING': (state, action) => set(lensProp('galleryScalingMode'), action.payload)(state)
}, initialState);
