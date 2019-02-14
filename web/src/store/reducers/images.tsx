import { image } from '../../types/image';

export interface ImageState {
    images: image[];
}

const initialState: ImageState = {
    images: [
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "2398fh23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "848djslk", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
        { "hash": "asd98u23", tags: [], uploader: "Reisen", createdAt: "01-02-1999", resolution: "800x600" },
    ]
};

export const imageReducer = (state: ImageState = initialState, action: any) => {
    return state;
};
