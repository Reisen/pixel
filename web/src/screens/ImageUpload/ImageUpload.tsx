import React, { useState, useRef } from 'react';
import { connect }                 from 'react-redux';
import { Image as image }          from '../../api/types';
import { State }                   from '../../store';
import { getImages }               from '../../store/images';
import { logoutUser }              from '../../store/users';
import { uploadImage }             from '../../api/images';
import { History }                 from 'history'

// Components
import Tag                         from '../../components/Tag';
import Button                      from '../../components/Button';
import TextInput                   from '../../components/TextInput';
import NavigationBar               from '../../components/NavigationBar';
import ImageInput                  from './components/ImageInput';
import UploadGrid                  from './components/UploadGrid';
import styles                      from './ImageUpload.module.css';


interface Props {
    images:     image[];
    logoutUser: () => void;
    username:   string;
    history:    History;
}

const headerLinks = [
    {name: 'Single Image', path: '/my/upload'},
    {name: 'Zip Upload',   path: '/my/upload/zip'}
];

const Image = (props: Props) => {
    // Create State Stuff
    const [tags, setTags]           = useState<string[]>([]);
    const [input, setInput]         = useState<string>('');
    const [uploading, setUploading] = useState<boolean>(false);
    const [image, setImage]         = useState<string | null>(null);

    // We need a Ref to the file element so we can capture events from the DOM
    // when it is set. This allows is to capture the image data when a file is
    // chosen and render it as a preview.
    const ref = useRef<HTMLInputElement>(null);

    // TODO: Extract Into Helper
    const suggestions: string[] = props.images.reduce((all: string[], image) => {
        return [...all, ...image.tags];
    }, []);

    // Do Tag Completion
    const suggestionWord = suggestions.find((tag: string) => {
        const words = input.split(' ');
        const word  = words[words.length - 1];
        return tag.startsWith(word);
    });

    const suggestion = input !== ''
        ? input.split(' ').slice(0, -1).join(' ') + ' ' + (suggestionWord || '')
        : '';

    // Create Event Handlers
    const onTagsChange = (e: React.ChangeEvent<HTMLInputElement>) =>
        setInput(e.target.value);

    const onTagsKeyPress = (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'Enter') {
            const merged = [...tags, ...input.toLowerCase().trim().split(' ')];
            const unique = new Set(merged);
            const sorted = Array.from(unique).sort();
            setTags(sorted);
            setInput('');
        }

        if (e.key === ' ' && suggestion && suggestionWord && suggestionWord !== '') {
            setInput(suggestion.trim());
        }
    };

    // Remove tags
    const removeTag = (k: number) => {
        tags.splice(k,1);
        setTags([...tags]);
    }

    // Handle Image Upload
    const startUpload = async () => {
        if (ref.current && ref.current.files) {
            setUploading(true);

            // Upload Each File
            for(let i = 0; i < ref.current.files.length; ++i) {
                // Create FormData containing the image and tags.
                const data = new FormData();
                data.append('image', ref.current.files[i]);
                tags.map(tag => {
                    data.append('tag', tag);
                    return null;
                });

                await uploadImage(data)
            }

            props.history.push('/');
        }
    }

    // Render Time!
    return (
        <div className="Page">
            <NavigationBar links={headerLinks} username={props.username} />

            <div className={styles.Root}>
                <div className={styles.TagManager}>
                    <Button onClick={startUpload} >Start Upload</Button>
                    <TextInput placeholder="Enter Tags"/>

                    <h1 className="Title--underline">Tags</h1>
                    <Tag icon="tag" name="dog" value="" onIcon={() => {}}/>
                    <Tag icon="tag" name="glasses" value="" onIcon={() => {}}/>
                    <Tag icon="tag" name="too_cool_for_school" value="" onIcon={() => {}}/>
                    <Tag icon="tag" name="barknold_schwarznegger" value="" onIcon={() => {}}/>
                    <Tag icon="tag" name="admin" value="" onIcon={() => {}}/>

                    <h1 className="Title--underline">Tags (Selection)</h1>
                    <Tag icon="tag" name="hotdog" value="" onIcon={() => {}}/>
                    <Tag icon="tag" name="hotterdog" value="" onIcon={() => {}}/>
                </div>

                <UploadGrid ref={ref} />
            </div>
        </div>
    );
}

const mapState = (state: State) => ({
    images:   getImages(state.images),
    username: state.user.username
});

const mapDispatch = {
    logoutUser
};


export default connect(mapState, mapDispatch)(Image);
