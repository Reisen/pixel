import React, { useState, useRef } from 'react';
import { State }                   from '../../store';
import { connect }                 from 'react-redux';
import { fetchImages }             from '../../store/images';
import { uploadImage }             from '../../api/images';
import { History }                 from 'history'

import Attribute                   from '../../components/Attribute';
import Button                      from '../../components/Button';
import TextInput                   from '../../components/TextInput';
import NavigationBar               from '../../components/NavigationBar';
import styles                      from './ImageUpload.module.css';


interface Props {
    username: string;
    history:  History;
}

const Image = (props: Props) => {
    // Create State Stuff
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
            tags.map(tag => data.append('tag', tag));
            uploadImage(data).then(() => {
                props.history.push('/');
            });
        }
    }

    // Render Time!
    return (
        <div className="Page">
            <NavigationBar username={props.username} />

            <div className={styles.Root}>
                <div className={styles.Tags}>
                    <TextInput
                        value={input}
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
    username: state.user.username
});

const mapDispatch = {
    fetchImages
};


export default connect(mapState, mapDispatch)(Image);
