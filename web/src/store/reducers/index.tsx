import { ImageState, imageReducer } from './images';
import { UserState, userReducer } from './user';

// Define the type of the full Redux state for the application.
export interface State {
    images: ImageState,
    user: UserState
}

export default {
    images: imageReducer,
    user: userReducer
};
