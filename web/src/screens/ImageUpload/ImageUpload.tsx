import React, { useState, useRef } from 'react';
import { Image as image }          from '../../api/types';
import { State }                   from '../../store';
import { connect }                 from 'react-redux';
import { getImages }               from '../../store/images';
import { uploadImage }             from '../../api/images';
import { History }                 from 'history'

import Attribute                   from '../../components/Attribute';
import Button                      from '../../components/Button';
import TextInput                   from '../../components/TextInput';
import NavigationBar               from '../../components/NavigationBar';
import styles                      from './ImageUpload.module.css';


interface Props {
    images:   image[];
    username: string;
    history:  History;
}

const Image = (props: Props) => {
    // Create State Stuff
    const allTags: string[] = props.images.reduce((all: string[], image) => {
        return [...all, ...image.tags];
    }, []);

    const fileElement       = useRef<HTMLInputElement>(null);
    const [tags, setTags]   = useState<string[]>([]);
    const [input, setInput] = useState<string>('');
    const [image, setImage] = useState<string | null>(null);

    // Create Event Handlers
    const onTagsChange = (e: React.ChangeEvent<HTMLInputElement>) =>
        setInput(e.target.value);

    const onTagsKeyPress = (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'Enter') {
            setTags([...tags, input.toLowerCase()].sort());
            setInput('');
        }
    };

    const onImageChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const reader = new FileReader();
        const files  = e.target.files;
        reader.onload = () => setImage(reader.result && reader.result.toString());
        files && reader.readAsDataURL(files[0]);
    };

    const onImageUpload = () => {
        const current = fileElement.current
        if (current && current.files) {
            const data = new FormData();
            data.append('image', current.files[0]);
            tags.map(tag => {
                data.append('tag', tag);
                console.log('Tag Added: ' + tag);
                return null;
            });
            uploadImage(data).then(() => {
                props.history.push('/');
            });
        }
    }

    // Do Tag Completion
    const suggestion = allTags.find((tag: string) => {
        const words = input.split(' ');
        const word  = words[words.length - 1];
        return tag.startsWith(word);
    });

    const completeTag = input === ''
        ? ''
        : input.split(' ').slice(0, -1).join(' ') + ' ' + (suggestion || '');

    // Render Time!
    return (
        <div className="Page">
            <NavigationBar username={props.username} />

            <div className={styles.Root}>
                <div className={styles.Tags}>
                    <TextInput
                        value={input}
                        suggestion={completeTag && completeTag.trim()}
                        onChange={onTagsChange}
                        placeholder="Enter Tags Here"
                        onKeyPress={onTagsKeyPress}
                    />

                    <div className={styles.TagList}>
                        {
                            tags.map((tag, k) =>
                                <Attribute key={k} icon="tag" name={tag} value="" />
                            )
                        }
                    </div>
                </div>

                <div className={styles.Uploader}>
                    <div className={styles.UploadManager} style={image ? {backgroundImage: `url(${image})`} : {}}>
                        {
                            !image && <span>Drag Image Here or Click</span>
                        }

                        <input
                            accept="image/*"
                            ref={fileElement}
                            type="file"
                            onChange={onImageChange}
                        />
                    </div>

                    <Button onClick={onImageUpload} disabled={!image} icon="upload">
                        Upload
                    </Button>
                </div>
            </div>
        </div>
    );
}

const mapState = (state: State) => ({
    images:   getImages(state.images),
    username: state.user.username
});

export default connect(mapState)(Image);
