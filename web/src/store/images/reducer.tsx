import { State }                  from './types';
import { handleActions }          from 'redux-actions';
import { compose, lensProp, set } from 'ramda';

const initialState: State = {
    images: [
        // Page 1
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/v6ug5bnh51h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#1", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/uama23jb40h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#2", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#3", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#4", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/5agvjiyyfwg21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#5", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#6", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#7", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/uama23jb40h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#8", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#9", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/v6ug5bnh51h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#10", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#11", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/5agvjiyyfwg21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#12", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#13", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/uama23jb40h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#14", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/v6ug5bnh51h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#15", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#16", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/5agvjiyyfwg21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#17", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#18", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "2398fh23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/uama23jb40h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#19", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/v6ug5bnh51h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#20", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#21", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#22", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/5agvjiyyfwg21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#23", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#24", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        // Page 2
        { "hash": "2398fh23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/uama23jb40h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#25", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/v6ug5bnh51h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#26", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#27", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#28", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/5agvjiyyfwg21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#29", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#30", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#31", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/uama23jb40h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#32", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#33", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/v6ug5bnh51h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#34", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/m8dbf8tsn4h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#35", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/5agvjiyyfwg21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#36", tags: ["dogs", "pussy", "yoshimitsu"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },

        { "hash": "asd98u23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/v6ug5bnh51h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#37", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", path: "https://external-preview.redd.it/O4mqVLwbIsnfKlqXAzRm2sm2YzNKICF708fCvpeFxHI.jpg?auto=webphttps://i.redd.it/uama23jb40h21.jpg#s=44468fa6cfb2373a61cc8c50ef9d469dc3ebf61d#38", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", path: "https://i.imgur.com/kblUAXs.jpg#39", tags: ["dogs", "cats"], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
    ],
};

export const imageReducer = handleActions<State>({
    'LOAD_IMAGES': (state, action) =>
        set(lensProp('images'), action.payload)(state)

}, initialState);
