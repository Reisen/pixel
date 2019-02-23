import { ImageState, imageReducer } from './images';
import { UserState, userReducer } from './user';


// State represents the full state type for our entire Redux store once it
// has been fully composed. Note that this is only our defined types, things
// injected into the state, such as by redux-router, aren't typed here.
export interface State {
    images: ImageState,
    user:   UserState,
}


export const reducers = {
    images: imageReducer,
    user:   userReducer
};
