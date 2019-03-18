import { State }                  from './types';
import { handleActions }          from 'redux-actions';
import { compose, lensProp, set } from 'ramda';

const initialState: State = {
    fillGrid: 'cover',
    images: [
        // Page 1
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg#1", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg#2", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#3", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#4", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg#5", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#6", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#7", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg#8", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/sv05qpF.jpg#9", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg#10", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#11", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg#12", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#13", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg#14", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg#15", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#16", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg#17", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#18", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg#19", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg#20", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#21", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#22", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg#23", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#24", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        // Page 2
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg#25", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg#26", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#27", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#28", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg#29", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#30", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#31", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg#32", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#33", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg#34", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.imgur.com/ysHH7a8.jpg#35", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://i.redd.it/5agvjiyyfwg21.jpg#36", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "asd98u23", path: "https://i.redd.it/v6ug5bnh51h21.jpg#37", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://i.redd.it/uama23jb40h21.jpg#38", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#39", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
    ],
};

export const imageReducer = handleActions<State>({
    'LOAD_IMAGES':       (state, action) => set(lensProp('images'), action.payload)(state),
    'SET_IMAGE_SCALING': (state, action) => set(lensProp('fillGrid'), action.payload)(state)

}, initialState);
