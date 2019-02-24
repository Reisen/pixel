import { State }                  from './types';
import { handleActions }          from 'redux-actions';
import { compose, lensProp, set } from 'ramda';

const initialState: State = {
    images: [
        // Page 1
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        // Page 2
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/m8dbf8tsn4h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
    ],
};

export const imageReducer = handleActions<State>({
    'LOAD_IMAGES': (state, action) =>
        set(lensProp('images'), action.payload)(state)

}, initialState);
