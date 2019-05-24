import React  from 'react';
import styles from './TextInput.module.css';


interface Props {
    icon?:       string;
    placeholder: string;
    suggestion?: string;
    value?:      string;
    type?:       string;
    onChange?:   (e: React.ChangeEvent<HTMLInputElement>) => void;
    onKeyPress?: (e: React.KeyboardEvent<HTMLInputElement>) => void;
}

const TextInput = (props: Props) => (
    <div className={styles.Root}>
        <input
            readOnly
            tabIndex={-1}
            className={styles.ShadowInput}
            value={props.suggestion}
        />

        <input
            type={props.type || 'text'}
            value={props.value}
            onChange={props.onChange}
            onKeyPress={props.onKeyPress}
            className={styles.TextInput}
            placeholder={props.placeholder}
        />
    </div>
);


export default TextInput;
