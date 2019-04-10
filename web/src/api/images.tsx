import 'whatwg-fetch';
import Cookies from 'universal-cookie';

// These Types are automatically generated from the back-end, if the back-end
// updates this file should then automatically break if it goes out of sync
// with the API.
import {
    DeleteTagsRequest,
    GetImageResponse,
    GetTagsResponse,
    PostTagsRequest
} from './types';


// HACK, BIG HACK
const findApiBase = () => {
    const cookies = new Cookies();
    return cookies.get('api') || 'http://localhost:3001';
};

// Fetch paginated images from the back-end when viewing a gallery. This only
// returns JSON data _about_ images, the images themselves are still loaded
// by standard <img> / background-url in various components.
const getImages = async (): Promise<GetImageResponse> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'GET',
    };

    const base      = findApiBase();
    const response  = await fetch(`${base}/api/image`, request);
    const json      = await response.json();
    return await json;
}

// Send a list of tags to remove from an image. Removing tags that aren't
// already on the image does nothing.
const deleteTags = async (uuid: string, req: DeleteTagsRequest): Promise<void> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'POST',
    };

    const base = findApiBase();
    fetch(`${base}/api/image/${uuid}/tags`, request);
}

// Retrieve a list of tags for a specific image, allows looking up tag data
// on pages where we already cached the image data but still want up to date
// tags, such as image editing.
const getTags = async (uuid: string): Promise<GetTagsResponse> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'GET',
    };

    const base     = findApiBase();
    const response = await fetch(`${base}/api/image/${uuid}/tags`, request);
    const json     = await response.json();
    return await json;
}

// Upload a new Image, this is done through a FormData object rather than
// JSON using multipart/form-data to upload the images.
const uploadImage = async (req: FormData): Promise<Response> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'POST',
        body:    req
    };

    const base = findApiBase();
    return fetch(`${base}/api/image`, request);
}

// Send new tags to attach to an image, any tags that already exist on the
// image are ignored.
const uploadTags = async (uuid: string, req: PostTagsRequest): Promise<void> => {
    const request = {
        headers: { 'Authorization': '96033b4a-44b0-4c14-ac44-890330b9877e' },
        method:  'POST',
    };

    const base = findApiBase();
    fetch(`${base}/api/image/${uuid}/tags`, request);
}

export {
    getImages,
    deleteTags,
    getTags,
    uploadImage,
    uploadTags
}
