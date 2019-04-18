import React, { useState } from 'react';

import Tag                 from '../../../components/Tag';
import TextInput           from '../../../components/TextInput';
import styles              from './TagPanel.module.css';


interface Props {
    tags?: [string, number][];
}

export default {
    name:    'tags',
    icon:    'tag',
    tooltip: 'Tag List',
    panel:   (props: Props) => {
        const [editMode, setEditMode] = useState<boolean>(false);
        const [deletes, setDeletes]   = useState<string[]>([]);
        const [adds, setAdds]         = useState<string[]>([]);
        const [input, setInput]       = useState<string>('');

        const toggleEditMode = () => {
            setDeletes([]);
            setInput('');
            setEditMode(!editMode)
        }

        const onTagsKeyPress = (e: React.KeyboardEvent<HTMLInputElement>) => {
            if (e.key === 'Enter') {
                setAdds([input, ...adds]);
                setInput('');
            }
        };

        return (
            <div>
                <h1>
                    Taglist
                    <span onClick={toggleEditMode} className={styles.EditButton}>
                        ({
                            deletes.length > 0 ? 'Save Changes' :
                            adds.length > 0    ? 'Save Changed' :
                            editMode           ? 'Cancel'
                                               : 'Edit'
                        })
                    </span>
                </h1>

                { editMode &&
                    <TextInput
                        value={input}
                        onChange={(e) => setInput(e.target.value)}
                        placeholder="Enter Tags Here"
                        onKeyPress={onTagsKeyPress}
                    />
                }


                <div className={styles.TagList}>
                    {
                        adds && adds.map(add => (
                            <Tag
                                key={add}
                                icon="tag"
                                name={add}
                                value="NEW"
                                onIcon={() => {}}
                            />
                        ))
                    }

                    {
                        props.tags && props.tags.map(tag => (
                            <Tag
                                onIcon={() => {
                                    setDeletes([tag[0], ...deletes])
                                }}
                                key={tag[0]}
                                icon={editMode ? "close-circled" : "tag"}
                                name={tag[0]}
                                strikethrough={deletes.includes(tag[0])}
                                value={tag[1].toString()}
                            />
                        ))
                    }
                </div>
            </div>
        )
    }
};
